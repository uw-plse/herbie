#lang racket

(require "../utils/common.rkt"
         "../utils/alternative.rkt"
         "programs.rkt"
         "../utils/timeline.rkt"
         "../syntax/types.rkt"
         "../utils/errors.rkt"
         "points.rkt"
         "../utils/float.rkt"
         "compiler.rkt")
(provide pareto-regimes
         (struct-out option)
         (struct-out si))

(module+ test
  (require rackunit
           "../syntax/load-plugin.rkt"
           "../syntax/syntax.rkt"
           "../syntax/sugar.rkt")
  (load-herbie-builtins))

(struct option (split-indices alts pts expr errors)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc opt port mode)
     (fprintf port "#<option ~a>" (option-split-indices opt)))])

(define (pareto-regimes sorted ctx)
  (define err-lsts (batch-errors (map alt-expr sorted) (*pcontext*) ctx))
  (define branches
    (if (null? sorted)
        '()
        (exprs-to-branch-on sorted ctx)))
  (define branch-exprs
    (if (flag-set? 'reduce 'branch-expressions)
        branches
        (context-vars ctx)))
  (let loop ([alts sorted]
             [errs (hash)]
             [err-lsts err-lsts])
    (cond
      [(null? alts) '()]
      ; Only return one option if not pareto mode
      [(and (not (*pareto-mode*)) (not (equal? alts sorted))) '()]
      [else
       (define-values (opt new-errs) (infer-splitpoints branch-exprs alts err-lsts #:errs errs ctx))
       (define high (si-cidx (argmax (λ (x) (si-cidx x)) (option-split-indices opt))))
       (cons opt (loop (take alts high) new-errs (take err-lsts high)))])))

;; `infer-splitpoints` and `combine-alts` are split so the mainloop
;; can insert a timeline break between them.

(define (infer-splitpoints branch-exprs alts err-lsts* #:errs [cerrs (hash)] ctx)
  (timeline-event! 'regimes)
  (timeline-push! 'inputs (map (compose ~a alt-expr) alts))
  (define sorted-bexprs
    (sort branch-exprs (lambda (x y) (< (hash-ref cerrs x -1) (hash-ref cerrs y -1)))))
  (define err-lsts (flip-lists err-lsts*))

  ;; invariant:
  ;; errs[bexpr] is some best option on branch expression bexpr computed on more alts than we have right now.
  (define-values (best best-err errs)
    (for/fold ([best '()]
               [best-err +inf.0]
               [errs cerrs]
               #:result (values best best-err errs))
              ([bexpr sorted-bexprs]
               ;; stop if we've computed this (and following) branch-expr on more alts and it's still worse
               #:break (> (hash-ref cerrs bexpr -1) best-err))
      (define opt (option-on-expr alts err-lsts bexpr ctx))
      (define err
        (+ (errors-score (option-errors opt))
           (length (option-split-indices opt)))) ;; one-bit penalty per split
      (define new-errs (hash-set errs bexpr err))
      (if (< err best-err)
          (values opt err new-errs)
          (values best best-err new-errs))))

  (timeline-push! 'count (length alts) (length (option-split-indices best)))
  (timeline-push! 'outputs
                  (for/list ([sidx (option-split-indices best)])
                    (~a (alt-expr (list-ref alts (si-cidx sidx))))))
  (timeline-push! 'baseline (apply min (map errors-score err-lsts*)))
  (timeline-push! 'accuracy (errors-score (option-errors best)))
  (define repr (context-repr ctx))
  (timeline-push! 'repr (~a (representation-name repr)))
  (timeline-push! 'oracle (errors-score (map (curry apply max) err-lsts)))
  (values best errs))

(define (exprs-to-branch-on alts ctx)
  (define alt-critexprs
    (for/list ([alt (in-list alts)])
      (all-critical-subexpressions (alt-expr alt) ctx)))
  (define start-critexprs (all-critical-subexpressions (*start-prog*) ctx))
  ;; We can only binary search if the branch expression is critical
  ;; for all of the alts and also for the start prgoram.
  (filter (λ (e) (equal? (representation-type (repr-of e ctx)) 'real))
          (set-intersect start-critexprs (apply set-union alt-critexprs))))

;; Requires that expr is not a λ expression
(define (critical-subexpression? expr subexpr)
  (define crit-vars (free-variables subexpr))
  (define replaced-expr (replace-expression expr subexpr 1))
  (define non-crit-vars (free-variables replaced-expr))
  (and (not (null? crit-vars)) (set-disjoint? crit-vars non-crit-vars)))

;; Requires that prog is a λ expression
(define (all-critical-subexpressions expr ctx)
  ;; We append all variables here in case of (λ (x y) 0) or similar,
  ;; where the variables do not appear in the body but are still worth
  ;; splitting on
  (for/list ([subexpr (set-union (context-vars ctx) (all-subexpressions expr))]
             #:when (critical-subexpression? expr subexpr))
    subexpr))

(define (option-on-expr alts err-lsts expr ctx)
  (define timeline-stop! (timeline-start! 'times (~a expr)))

  (define fn (compile-prog expr ctx))
  (define repr (repr-of expr ctx))

  (define big-table ; pt ; splitval ; alt1-err ; alt2-err ; ...
    (for/list ([(pt ex) (in-pcontext (*pcontext*))]
               [err-lst err-lsts])
      (list* (apply fn pt) pt err-lst)))
  (match-define (list splitvals* pts* err-lsts* ...)
    (flip-lists (sort big-table (curryr </total repr) #:key first)))

  (define bit-err-lsts* (map (curry map ulps->bits) err-lsts*))

  (define can-split?
    (cons #f
          (for/list ([val (cdr splitvals*)]
                     [prev splitvals*])
            (</total prev val repr))))
  (define split-indices (infer-split-indices bit-err-lsts* can-split?))
  (define out (option split-indices alts pts* expr (pick-errors split-indices pts* err-lsts* repr)))
  (timeline-stop!)
  (timeline-push! 'branch
                  (~a expr)
                  (errors-score (option-errors out))
                  (length split-indices)
                  (~a (representation-name repr)))
  out)

(define/contract (pick-errors split-indices pts err-lsts repr)
  (->i ([sis (listof si?)] [vss (r) (listof (listof (representation-repr? r)))]
                           [errss (listof (listof real?))]
                           [r representation?])
       [idxs (listof nonnegative-integer?)])
  (for/list ([i (in-naturals)]
             [pt pts]
             [errs (flip-lists err-lsts)])
    (for/first ([si split-indices]
                #:when (< i (si-pidx si)))
      (list-ref errs (si-cidx si)))))

(module+ test
  (define ctx (make-debug-context '(x)))
  (parameterize ([*start-prog* (literal 1 'binary64)]
                 [*pcontext* (mk-pcontext '((0.5) (4.0)) '(1.0 1.0))])
    (define alts (map make-alt (list '(fmin.f64 x 1) '(fmax.f64 x 1))))
    (define err-lsts `((,(expt 2.0 53) 1.0) (1.0 ,(expt 2.0 53))))

    (define (test-regimes expr goal)
      (check (lambda (x y) (equal? (map si-cidx (option-split-indices x)) y))
             (option-on-expr alts err-lsts expr ctx)
             goal))

    ;; This is a basic sanity test
    (test-regimes 'x '(1 0))

    ;; This test ensures we handle equal points correctly. All points
    ;; are equal along the `1` axis, so we should only get one
    ;; splitpoint (the second, since it is better at the further point).
    (test-regimes (literal 1 'binary64) '(0))

    (test-regimes `(if (==.f64 x ,(literal 0.5 'binary64))
                       ,(literal 1 'binary64)
                       (NAN.f64))
                  '(1 0))))

;; Given error-lsts, returns a list of sp objects representing where the optimal splitpoints are.
(define (valid-splitindices? can-split? split-indices)
  (and (for/and ([pidx (map si-pidx (drop-right split-indices 1))])
         (and (> pidx 0) (list-ref can-split? pidx)))
       (= (si-pidx (last split-indices)) (length can-split?))))

(module core typed/racket
  (provide (struct-out si)
           infer-split-indices)
  (require math/flonum)

  ;; Struct representing a splitindex
  ;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
  ;; pidx = Point index: The index of the point to the left of which we should split.
  (struct si ([cidx : Integer] [pidx : Integer]) #:prefab)

  ;; This is the core main loop of the regimes algorithm.
  ;; Takes in a list of alts in the form of there error at a given point
  ;; as well as a list of split indices to determine when it's ok to split
  ;; for another alt.
  ;; Returns a list of split indices saying which alt to use for which
  ;; range of points. Starting at 1 going up to num-points.
  ;; Alts are indexed 0 and points are index 1.
  (: infer-split-indices (-> (Listof (Listof Flonum)) (Listof Boolean) (Listof si)))
  (define (infer-split-indices err-lsts can-split)
    ;; Coverts the list to vector form for faster processing
    (define can-split-vec (list->vector can-split))
    ;; Converting list of list to list of flvectors
    ;; flvectors are used to remove pointer chasing
    (define (make-vec-psum [lst : (Listof Flonum)])
      (flvector-sums (list->flvector lst)))
    (define flvec-psums (vector-map make-vec-psum (list->vector err-lsts)))

    ;; Set up data needed for algorithm
    (define number-of-points (vector-length can-split-vec))
    ;; min-weight is used as penalty to favor not adding split points
    (define min-weight (fl number-of-points))

    ;; These 3 vectors are will contain the output data and be used for
    ;; determining which alt is best for a given point
    (define result-error-sums (make-flvector number-of-points +inf.0))
    (define result-alt-idxs (make-vector number-of-points 0))
    (define result-prev-idxs (make-vector number-of-points number-of-points))

    (for ([alt-idx (in-naturals)]
          [alt-errors (in-vector flvec-psums)])
      (for ([point-idx (in-range number-of-points)]
            [err (in-flvector alt-errors)]
            #:when (< err (flvector-ref result-error-sums point-idx)))
        (flvector-set! result-error-sums point-idx err)
        (vector-set! result-alt-idxs point-idx alt-idx)))

    ;; Vectors are now filled with starting data. Beginning main loop of the
    ;; regimes algorithm.

    ;; Vectors used to determine if our current alt is better than our running
    ;; best alt.
    (: best-alt-idxs (Vectorof Integer))
    (: best-alt-costs FlVector)
    (define best-alt-idxs (make-vector number-of-points -1))
    (define best-alt-costs (make-flvector number-of-points))

    (for ([point-idx (in-range 0 number-of-points)]
          [current-alt-error (in-flvector result-error-sums)]
          [current-alt-idx (in-vector result-alt-idxs)]
          [current-prev-idx (in-vector result-prev-idxs)])
      ;; Set and fill temporary vectors with starting data
      ;; #f for best index and positive infinite for best cost
      (vector-fill! best-alt-idxs -1)
      (set! best-alt-costs (make-flvector number-of-points +inf.0))

      ;; For each alt loop over its vector of errors
      (for ([alt-idx (in-naturals)]
            [alt-error-sums (in-vector flvec-psums)])
        ;; Loop over the points up to our current point
        (for ([prev-split-idx (in-range 0 point-idx)]
              [prev-alt-error-sum (in-flvector alt-error-sums)]
              [best-alt-idx (in-vector best-alt-idxs)]
              [best-alt-cost (in-flvector best-alt-costs)]
              [can-split (in-vector can-split-vec 1)]
              #:when can-split)
          ;; Check if we can add a split point
          ;; compute the difference between the current error-sum and previous
          (let ([current-error (- (flvector-ref alt-error-sums point-idx) prev-alt-error-sum)])
            ;; if we have not set the best alt yet or
            ;; the current alt-error-sum is less then previous
            (when (or (= best-alt-idx -1) (< current-error best-alt-cost))
              ;; update best cost and best index
              (flvector-set! best-alt-costs prev-split-idx current-error)
              (vector-set! best-alt-idxs prev-split-idx alt-idx)))))
      ;; We have now have the index of the best alt and its error up to our
      ;; current point-idx.
      ;; Now we compare against our current best saved in the 3 vectors above
      (for ([prev-split-idx (in-range 0 point-idx)]
            [r-error-sum (in-flvector result-error-sums)]
            [best-alt-idx (in-vector best-alt-idxs)]
            [best-alt-cost (in-flvector best-alt-costs)]
            [can-split (in-vector can-split-vec 1)]
            #:when can-split)
        ;; Re compute the error sum for a potential better alt
        (define alt-error-sum (+ r-error-sum best-alt-cost min-weight))
        ;; Check if the new alt-error-sum is better then the current
        (define set-cond
          ;; give benefit to previous best alt
          (cond
            [(< alt-error-sum current-alt-error) #t]
            ;; Tie breaker if error are the same favor first alt
            [(and (= alt-error-sum current-alt-error) (> current-alt-idx best-alt-idx)) #t]
            ;; Tie breaker for if error and alt is the same
            [(and (= alt-error-sum current-alt-error)
                  (= current-alt-idx best-alt-idx)
                  (> current-prev-idx prev-split-idx))
             #t]
            [else #f]))
        (when set-cond
          (set! current-alt-error alt-error-sum)
          (set! current-alt-idx best-alt-idx)
          (set! current-prev-idx prev-split-idx)))
      (flvector-set! result-error-sums point-idx current-alt-error)
      (vector-set! result-alt-idxs point-idx current-alt-idx)
      (vector-set! result-prev-idxs point-idx current-prev-idx))

    ;; Loop over results vectors in reverse and build the output split index list
    (let loop ([i (- number-of-points 1)]
               [rest (cast null (Listof si))])
      (define alt-idx (vector-ref result-alt-idxs i))
      (define next (vector-ref result-prev-idxs i))
      (define sis (cons (si alt-idx (+ i 1)) rest))
      (if (< next i)
          (loop next sis)
          sis))))

(require (submod "." core))
