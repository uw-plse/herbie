#lang racket

(require math/bigfloat)
(require "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "rules.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/platform.rkt"
         "../syntax/read.rkt"
         "../syntax/read.rkt"
         "../core/rival.rkt"
         "points.rkt"
         "programs.rkt"
         "sampling.rkt"
         "simplify.rkt"
         "egg-herbie.rkt"
         "compiler.rkt"
         "batch.rkt")

(module+ test
  (require rackunit
           "../syntax/load-plugin.rkt"
           "../syntax/syntax.rkt"
           "../syntax/sugar.rkt")
  (load-herbie-builtins))

(provide batch-localize-costs
         batch-localize-errors
         compute-local-errors
         local-error-as-tree)

(define (regroup-nested inputss outputs)
  (match* (inputss outputs)
    [((cons (cons fhead ftail) rest) (cons head tail))
     (match-define (cons fout out) (regroup-nested (cons ftail rest) tail))
     (cons (cons head fout) out)]
    [((cons '() rest) outputs) (cons '() (regroup-nested rest outputs))]
    [('() '()) '()]))

(define (fraction-with-odd-denominator? frac)
  (and (rational? frac) (let ([denom (denominator frac)]) (and (> denom 1) (odd? denom)))))

(define (pow-impl-args impl args)
  (define vars (impl-info impl 'vars))
  (match (impl-info impl 'spec)
    [(list 'pow b e)
     #:when (set-member? vars e)
     (define env (map cons vars args))
     (define b* (dict-ref env b b))
     (define e* (dict-ref env e e))
     (cons b* e*)]
    [_ #f]))

(define (default-cost-proc expr _)
  (let rec ([expr expr])
    (match expr
      [(literal _ _) 1]
      [(? symbol?) 1]
      ; approx node
      [(approx _ impl) (rec impl)]
      [(list 'if cond ift iff) (+ 1 (rec cond) (rec ift) (rec iff))]
      [(list (? impl-exists? impl) args ...)
       (match (pow-impl-args impl args)
         [(cons _ (literal e _))
          #:when (fraction-with-odd-denominator? e)
          +inf.0]
         [_ (apply + 1 (map rec args))])]
      [(list _ args ...) (apply + 1 (map rec args))])))

(define (batch-localize-costs exprs ctx)
  (define subexprss (map all-subexpressions exprs))
  (define progs (apply append subexprss))

  ; inputs to egg
  (define reprs (map (lambda (prog) (repr-of prog ctx)) progs))
  (define rules (real-rules (*simplify-rules*)))
  (define lifting-rules (platform-lifting-rules))
  (define lowering-rules (platform-lowering-rules))

  ; egg runner (2-phases for real rewrites and implementation selection)
  (define batch (progs->batch progs))
  (define runner
    (make-egg-runner batch
                     (batch-roots batch)
                     reprs
                     `((lift . ((iteration . 1) (scheduler . simple)))
                       (,rules . ((node . ,(*node-limit*))))
                       (lower . ((iteration . 1) (scheduler . simple))))))

  ; run egg
  (define simplified
    (map (compose debatchref last)
         (simplify-batch runner
                         (typed-egg-batch-extractor
                          (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc)
                          batch))))

  ; run egg
  (define simplifiedss (regroup-nested subexprss simplified))

  ; build map from starting expr to simplest
  (define expr->simplest (make-hash))
  (for ([subexprs (in-list subexprss)]
        [simplifieds (in-list simplifiedss)]
        #:when #t
        [subexpr (in-list subexprs)]
        [simplified (in-list simplifieds)])
    (hash-set! expr->simplest subexpr simplified))

  ; platform-based expression cost
  (define cost-proc
    (if (*egraph-platform-cost*)
        (platform-cost-proc (*active-platform*))
        default-cost-proc))
  (define (expr->cost expr)
    (cost-proc expr (repr-of expr ctx)))

  (define (cost-opportunity subexpr children)
    ; start and end cost of roots
    (define start-cost (expr->cost subexpr))
    (define best-cost (expr->cost (hash-ref expr->simplest subexpr)))
    ; start and end cost of children
    (define start-child-costs (map expr->cost children))
    (define best-child-costs
      (for/list ([child (in-list children)])
        (expr->cost (hash-ref expr->simplest child))))
    (unless (>= start-cost best-cost)
      (error 'cost-opportunity
             "Initial expression ~a is better than final expression ~a\n"
             subexpr
             (hash-ref expr->simplest subexpr)))

    ; Cost opportunity would normally be:
    ;   (start cost - start child costs) - (best cost - best child costs)
    ; However, we rearrange to handle infinities:
    (define a (apply + start-cost best-child-costs))
    (define b (apply + best-cost start-child-costs))
    (if (= a b)
        0
        (- a b))) ; This `if` statement handles `inf - inf`

  ; rank subexpressions by cost opportunity
  (define localize-costss
    (for/list ([subexprs (in-list subexprss)])
      (sort (reap [sow]
                  (for ([subexpr (in-list subexprs)])
                    (match subexpr
                      [(? literal?) (void)]
                      [(? symbol?) (void)]
                      [(approx _ impl)
                       (define cost-opp (cost-opportunity subexpr (list impl)))
                       (sow (cons cost-opp subexpr))]
                      [(list _ args ...)
                       (define cost-opp (cost-opportunity subexpr args))
                       (sow (cons cost-opp subexpr))])))
            >
            #:key car)))

  localize-costss)

(define (batch-localize-errors exprs ctx)
  (define subexprss (map all-subexpressions exprs))
  (define errss (compute-local-errors subexprss ctx))

  (for/list ([_ (in-list exprs)]
             [errs (in-list errss)])
    (sort (sort (for/list ([(subexpr err) (in-hash errs)]
                           #:when (or (list? subexpr) (approx? subexpr)))
                  (cons err subexpr))
                expr<?
                #:key cdr)
          >
          #:key (compose errors-score car))))

;; The local error of an expression f(x, y) is
;;
;;   R[f(x, y)] - f(R[x], R[y])
;;
;; where the `-` is interpreted as ULP difference and `E` means
;; exact real evaluation rounded to target repr.
;;
;; Local error is high when `f` is highly sensitive to rounding error
;; in its inputs `x` and `y`.

(define (local-error exact node repr get-exact)
  (match node
    [(? literal?) 1]
    [(? variable?) 1]
    [(approx _ impl) (ulp-difference exact (get-exact impl) repr)]
    [`(if ,c ,ift ,iff) 1]
    [(list f args ...)
     (define argapprox (map get-exact args))
     (define approx (apply (impl-info f 'fl) argapprox))
     (ulp-difference exact approx repr)]))

(define (make-matrix roots pcontext)
  (for/vector #:length (vector-length roots)
              ([node (in-vector roots)])
    (make-vector (pcontext-length (*pcontext*)))))

; Compute local error or each sampled point at each node in `prog`.
(define (compute-local-errors subexprss ctx)
  (define exprs-list (append* subexprss)) ; unroll subexprss
  (define reprs-list (map (curryr repr-of ctx) exprs-list))
  (define ctx-list
    (for/list ([subexpr (in-list exprs-list)]
               [repr (in-list reprs-list)])
      (struct-copy context ctx [repr repr])))

  (define expr-batch (progs->batch exprs-list))
  (define nodes (batch-nodes expr-batch))
  (define roots (batch-roots expr-batch))

  (define subexprs-fn (eval-progs-real (map prog->spec exprs-list) ctx-list))

  (define errs (make-matrix roots (*pcontext*)))

  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [pt-idx (in-naturals)])
    (define exacts (list->vector (apply subexprs-fn pt)))
    (define (get-exact idx)
      (vector-ref exacts (vector-member idx roots)))
    (for ([expr (in-list exprs-list)]
          [root (in-vector roots)]
          [repr (in-list reprs-list)]
          [exact (in-vector exacts)]
          [expr-idx (in-naturals)])
      (define err (local-error exact (vector-ref nodes root) repr get-exact))
      (vector-set! (vector-ref errs expr-idx) pt-idx err)))

  (define n 0)
  (for/list ([subexprs (in-list subexprss)])
    (for*/hash ([subexpr (in-list subexprs)])
      (begin0 (values subexpr (vector->list (vector-ref errs n)))
        (set! n (add1 n))))))

;; The absolute error of expression `e` is R[e - R[e]].
;; However, it's possible that R[e] is infinity or NaN;
;; in this case, computing the absolute error won't work
;; since those aren't real numbers. To fix this, we replace all
;; non-finite R[e] with 0.
(define (remove-infinities pt reprs)
  (for/list ([val (in-vector pt)]
             [repr (in-list reprs)])
    (define bf-val ((representation-repr->bf repr) val))
    (if (implies (bigfloat? bf-val) (bfrational? bf-val))
        val
        ((representation-bf->repr repr) 0.bf))))

;; Compute local error or each sampled point at each node in `prog`.
(define (compute-errors subexprss ctx)
  ;; We compute the actual (float) result
  (define exprs-list (append* subexprss)) ; unroll subexprss
  (define actual-value-fn (compile-progs exprs-list ctx))

  ;; And the real result
  (define spec-list (map prog->spec exprs-list))
  (define reprs-list (map (curryr repr-of ctx) exprs-list))
  (define ctx-list
    (for/list ([subexpr (in-list exprs-list)]
               [repr (in-list reprs-list)])
      (struct-copy context ctx [repr repr])))
  (define subexprs-fn (eval-progs-real spec-list ctx-list))

  ;; And the absolute difference between the two
  (define exact-var-names
    (for/list ([expr (in-list exprs-list)]
               [n (in-naturals)])
      ;; HACK: should generate unique, not just rare, symbol
      (string->symbol (format "-exact-for-~a" n))))
  (define delta-ctx
    (context (append (context-vars ctx) exact-var-names)
             (get-representation 'binary64)
             (append (context-var-reprs ctx) reprs-list)))
  (define compare-specs
    (for/list ([spec (in-list spec-list)]
               [expr (in-list exprs-list)]
               [repr (in-list reprs-list)]
               [var (in-list exact-var-names)])
      (cond
        [(number? spec) 0] ; HACK: unclear why numbers don't work in Rival but :shrug:
        [(equal? (representation-type repr) 'bool) 0] ; HACK: just ignore differences in booleans
        [else `(fabs (- ,spec ,var))])))
  (define delta-fn (eval-progs-real compare-specs (map (const delta-ctx) compare-specs)))

  (define expr-batch (progs->batch exprs-list))
  (define nodes (batch-nodes expr-batch))
  (define roots (batch-roots expr-batch))

  (define ulp-errs (make-matrix roots (*pcontext*)))
  (define exacts-out (make-matrix roots (*pcontext*)))
  (define approx-out (make-matrix roots (*pcontext*)))
  (define true-error-out (make-matrix roots (*pcontext*)))

  (define spec-vec (list->vector spec-list))
  (define ctx-vec (list->vector ctx-list))
  (for ([(pt ex) (in-pcontext (*pcontext*))]
        [pt-idx (in-naturals)])

    (define exacts (list->vector (apply subexprs-fn pt)))
    (define (get-exact idx)
      (vector-ref exacts (vector-member idx roots)))

    (define actuals (apply actual-value-fn pt))
    (define pt* (append pt (remove-infinities actuals reprs-list)))
    (define deltas (list->vector (apply delta-fn pt*)))

    (for ([repr (in-list reprs-list)]
          [root (in-vector roots)]
          [exact (in-vector exacts)]
          [actual (in-vector actuals)]
          [delta (in-vector deltas)]
          [expr-idx (in-naturals)])
      (define ulp-err (local-error exact (vector-ref nodes root) repr get-exact))
      (vector-set! (vector-ref exacts-out expr-idx) pt-idx exact)
      (vector-set! (vector-ref approx-out expr-idx) pt-idx actual)
      (vector-set! (vector-ref ulp-errs expr-idx) pt-idx ulp-err)
      (vector-set! (vector-ref true-error-out expr-idx) pt-idx delta)))

  (define n 0)
  (for/list ([subexprs (in-list subexprss)])
    (for*/hash ([subexpr (in-list subexprs)])
      (begin0 (values subexpr
                      (hasheq 'ulp-errs
                              (vector->list (vector-ref ulp-errs n))
                              'exact-values
                              (vector->list (vector-ref exacts-out n))
                              'approx-values
                              (vector->list (vector-ref approx-out n))
                              'absolute-error
                              (vector->list (vector-ref true-error-out n))))
        (set! n (add1 n))))))

(module+ test
  (define ctx (make-debug-context '(x y)))
  (define spec `(- (sqrt (+ x 1)) (sqrt y)))
  (define pt `(1e-100 1e-100))
  (define exact 1e-50)
  (check-equal? (first (apply (eval-progs-real (list `(- ,spec ,exact)) (list ctx)) pt)) 1.0))

(define (expr->spec-operator expr)
  (match expr
    [(list op args ...) op]
    [(? number? c) (exact->inexact c)]
    [(? variable? c) c]))

;; Compute the local error of every subexpression of `prog`
;; and returns the error information as an S-expr in the
;; same shape as `prog`
(define (local-error-as-tree expr ctx)
  (define data-hash (first (compute-errors (list (all-subexpressions expr)) ctx)))
  (define fpcore (prog->fpcore expr ctx))
  (define mapping
    (for/hash ([subexpr (in-list (all-subexpressions expr))]
               [subfpcore (in-list (all-subexpressions fpcore))])
      (values subexpr subfpcore)))

  (define (translate-booleans value)
    (match value
      [#t 'true]
      [#f 'false]
      [v v]))

  (define (make-hash-for expr)
    (define data (hash-ref data-hash expr))
    (define abs-error (~s (first (hash-ref data 'absolute-error))))
    (define ulp-error (~s (ulps->bits (first (hash-ref data 'ulp-errs))))) ; unused by Odyssey
    (define avg-error (format-bits (errors-score (hash-ref data 'ulp-errs))))
    (define exact-error (~s (translate-booleans (first (hash-ref data 'exact-values)))))
    (define actual-error (~s (translate-booleans (first (hash-ref data 'approx-values)))))
    (define percent-accurate
      (if (nan? (first (hash-ref data 'absolute-error)))
          'invalid ; HACK: should specify if invalid or unsamplable
          (let* ([repr (repr-of expr ctx)]
                 [total-bits (representation-total-bits repr)]
                 [bits-error (ulps->bits (first (hash-ref data 'ulp-errs)))])
            (* 100 (- 1 (/ bits-error total-bits))))))
    (hasheq 'e
            (~s (expr->spec-operator (hash-ref mapping expr)))
            'ulps-error
            ulp-error
            'avg-error
            avg-error
            'exact-value
            exact-error
            'actual-value
            actual-error
            'percent-accuracy
            (~s percent-accurate)
            'abs-error-difference
            (match (first (hash-ref data 'absolute-error))
              [(? zero?) "equal"]
              [(? nan?) "invalid"]
              [_ abs-error])
            'children
            (map make-hash-for
                 (if (list? expr)
                     (rest expr)
                     '()))))

  (make-hash-for expr))
