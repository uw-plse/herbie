#lang racket

(require "egg-herbie.rkt"
         "simplify.rkt"
         "rules.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/errors.rkt"
         "programs.rkt"
         "points.rkt"
         "../utils/timeline.rkt"
         "../utils/float.rkt"
         "batch.rkt")

(provide find-preprocessing
         preprocess-pcontext
         remove-unnecessary-preprocessing)

(define (has-fabs-neg-impls? repr)
  (and (get-fpcore-impl '- (repr->prop repr) (list repr))
       (get-fpcore-impl 'fabs (repr->prop repr) (list repr))))

(define (has-copysign-impl? repr)
  (get-fpcore-impl 'copysign (repr->prop repr) (list repr repr)))

;; The even identities: f(x) = f(-x)
;; Requires `neg` and `fabs` operator implementations.
(define (make-even-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))]
             [repr (in-list (context-var-reprs ctx))]
             #:when (has-fabs-neg-impls? repr))
    (list 'even var (replace-expression spec var `(neg ,var)))))

;; The odd identities: f(x) = -f(-x)
;; Requires `neg` and `fabs` operator implementations.
(define (make-odd-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))]
             [repr (in-list (context-var-reprs ctx))]
             #:when (and (has-fabs-neg-impls? repr) (has-copysign-impl? repr)))
    (list 'odd var (replace-expression `(neg ,spec) var `(neg ,var)))))

;; Swap identities: f(a, b) = f(b, a)
(define (make-swap-identities spec ctx)
  (define pairs (combinations (context-vars ctx) 2))
  (for/list ([pair (in-list pairs)])
    (match-define (list a b) pair)
    (list 'swap pair (replace-vars `((,a . ,b) (,b . ,a)) spec))))

;; Initial simplify
(define (initial-simplify expr ctx)
  (define rules (*simplify-rules*))
  (define lifting-rules (platform-lifting-rules))
  (define lowering-rules (platform-lowering-rules))

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    `((,lifting-rules . ((iteration . 1) (scheduler . simple)))
      (,rules . ((node . ,(*node-limit*))))
      (,lowering-rules . ((iteration . 1) (scheduler . simple)))))

  ; egg query
  (define batch (progs->batch (list expr)))
  (define runner (make-egg-runner batch (batch-roots batch) (list (context-repr ctx)) schedule))

  ; run egg
  (define simplified
    (simplify-batch runner
                    (typed-egg-batch-extractor
                     (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc)
                     batch)))

  ; alternatives
  (define start-alt (make-alt expr))
  (cons start-alt
        (remove-duplicates
         (for/list ([batchreff (rest simplified)])
           (alt (debatchref batchreff) `(simplify () ,runner #f #f) (list start-alt) '()))
         alt-equal?)))

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing init expr ctx)
  (define spec (prog->spec expr))

  ;; identities
  (define even-identities (make-even-identities spec ctx))
  (define odd-identities (make-odd-identities spec ctx))
  (define swap-identities (make-swap-identities spec ctx))
  (define identities (append even-identities odd-identities swap-identities))

  (define specs
    (for/list ([ident (in-list identities)])
      (match ident
        [(list 'even _ spec) spec]
        [(list 'odd _ spec) spec]
        [(list 'swap _ spec) spec])))

  ;; make egg runner
  (define rules (*simplify-rules*))

  (define batch (progs->batch specs))
  (define runner
    (make-egg-runner batch
                     (batch-roots batch)
                     (map (lambda (_) (context-repr ctx)) specs)
                     `((,rules . ((node . ,(*node-limit*)))))))

  ;; run egg to check for identities
  (define expr-pairs (map (curry cons spec) specs))
  (define equal?-lst (run-egg runner `(equal? . ,expr-pairs)))

  ;; collect equalities
  (define abs-instrs '())
  (define negabs-instrs '())
  (define swaps '())
  (for ([ident (in-list identities)]
        [expr-equal? (in-list equal?-lst)]
        #:when expr-equal?)
    (match ident
      [(list 'even var _) (set! abs-instrs (cons (list 'abs var) abs-instrs))]
      [(list 'odd var _) (set! negabs-instrs (cons (list 'negabs var) negabs-instrs))]
      [(list 'swap pair _) (set! swaps (cons pair swaps))]))

  (define components (connected-components (context-vars ctx) swaps))
  (define sort-instrs
    (for/list ([component (in-list components)]
               #:when (> (length component) 1))
      (cons 'sort component)))

  (define instrs (append abs-instrs negabs-instrs sort-instrs))
  (define start-alts
    (if (flag-set? 'setup 'simplify)
        ; initial simplify
        (for/list ([altn (initial-simplify init ctx)])
          (alt-add-preprocessing altn instrs))
        (list (make-alt-preprocessing init instrs))))

  (values start-alts instrs))

(define (connected-components variables swaps)
  (define components (disjoint-set (length variables)))
  (for ([swap (in-list swaps)])
    (match-define (list a b) swap)
    (disjoint-set-union! components
                         (disjoint-set-find! components (index-of variables a))
                         (disjoint-set-find! components (index-of variables b))))
  (group-by (compose (curry disjoint-set-find! components) (curry index-of variables)) variables))

(define (preprocess-pcontext context pcontext preprocessing)
  (define preprocess
    (apply compose
           (map (curry instruction->operator context)
                ;; Function composition applies the rightmost function first
                (reverse preprocessing))))
  (for/pcontext ([(x y) pcontext]) (preprocess x y)))

(define (instruction->operator context instruction)
  (define variables (context-vars context))
  (define sort* (curryr sort (curryr </total (context-repr context))))
  (match instruction
    [(list 'sort component ...)
     (unless (subsequence? component variables)
       (error 'instruction->operator "component should always be a subsequence of variables"))
     (define indices (indexes-where variables (curryr member component)))
     (lambda (x y)
       (let* ([subsequence (map (curry list-ref x) indices)]
              [sorted (sort* subsequence)])
         (values (list-set* x indices sorted) y)))]
    [(list 'abs variable)
     (define index (index-of variables variable))
     (define var-repr (context-lookup context variable))
     (define abs-proc (impl-info (get-fpcore-impl 'fabs (repr->prop var-repr) (list var-repr)) 'fl))
     (lambda (x y) (values (list-update x index abs-proc) y))]
    [(list 'negabs variable)
     (define index (index-of variables variable))
     (define var-repr (context-lookup context variable))
     (define neg-var (impl-info (get-fpcore-impl '- (repr->prop var-repr) (list var-repr)) 'fl))

     (define repr (context-repr context))
     (define neg-expr (impl-info (get-fpcore-impl '- (repr->prop repr) (list repr)) 'fl))

     (lambda (x y)
       ;; Negation is involutive, i.e. it is its own inverse, so t^1(y') = -y'
       (if (negative? (repr->real (list-ref x index) (context-repr context)))
           (values (list-update x index neg-var) (neg-expr y))
           (values x y)))]))

; until fixed point, iterate through preprocessing attempting to drop preprocessing with no effect on error
(define (remove-unnecessary-preprocessing expression
                                          context
                                          pcontext
                                          preprocessing
                                          #:removed [removed empty])
  (define-values (result newly-removed)
    (let loop ([preprocessing preprocessing]
               [i 0]
               [removed removed])
      (cond
        [(>= i (length preprocessing)) (values preprocessing removed)]
        [(preprocessing-<=? expression context pcontext (drop-at preprocessing i) preprocessing)
         (loop (drop-at preprocessing i) i (cons (list-ref preprocessing i) removed))]
        [else (loop preprocessing (+ i 1) removed)])))
  (cond
    [(< (length result) (length preprocessing))
     (remove-unnecessary-preprocessing expression context pcontext result #:removed newly-removed)]
    [else
     (timeline-push! 'remove-preprocessing (map ~a newly-removed))
     result]))

(define (preprocessing-<=? expression context pcontext preprocessing1 preprocessing2)
  (define pcontext1 (preprocess-pcontext context pcontext preprocessing1))
  (define pcontext2 (preprocess-pcontext context pcontext preprocessing2))
  (<= (errors-score (errors expression pcontext1 context))
      (errors-score (errors expression pcontext2 context))))
