#lang racket

(require math/bigfloat
         racket/hash)
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

(provide compute-local-errors
         local-error-as-tree)

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
    (for/list ([expr (in-list exprs-list)])
      (gensym 'exact)))
  (define delta-ctx
    (context (append (context-vars ctx) exact-var-names)
             (get-representation 'binary64)
             (append (context-var-reprs ctx) reprs-list)))
  (define compare-specs
    (for/list ([spec (in-list spec-list)]
               [expr (in-list exprs-list)]
               [repr (in-list reprs-list)]
               [var (in-list exact-var-names)])
      (match (representation-type repr)
        ['bool 0] ; We can't subtract booleans so ignore them
        ['real `(fabs (- ,spec ,var))])))
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

(define (expr-fpcore-operator expr ctx)
  (match (prog->fpcore expr ctx)
    [(list '! props ... (list op args ...)) op]
    [(list op args ...) op]
    [(? number? c) (exact->inexact c)]
    [(? variable? c) c]))

(define (expr->json-tree expr ctx decorate)
  (define (make-json-tree subexpr)
    (define args
      (if (list? subexpr)
          (rest subexpr)
          '()))
    (hash-union
     (hasheq 'e (~s (expr-fpcore-operator subexpr ctx)) 'children (map make-json-tree args))
     (decorate subexpr)))
  (make-json-tree expr))

;; Compute the local error of every subexpression of `prog`
;; and returns the error information as an S-expr in the
;; same shape as `prog`
(define (local-error-as-tree expr ctx)
  (define data-hash (first (compute-errors (list (all-subexpressions expr)) ctx)))

  (define (translate-booleans value)
    (match value
      [#t 'true]
      [#f 'false]
      [v v]))

  (define (expr-data expr)
    (define data (hash-ref data-hash expr))
    (define abs-error (~s (first (hash-ref data 'absolute-error))))
    (define ulp-error (~s (ulps->bits (first (hash-ref data 'ulp-errs))))) ; unused by Odyssey
    (define avg-error (format-bits (errors-score (hash-ref data 'ulp-errs))))
    (define exact-error (~s (translate-booleans (first (hash-ref data 'exact-values)))))
    (define actual-error (~s (translate-booleans (first (hash-ref data 'approx-values)))))
    (define percent-accurate
      (cond
        [(nan? (first (hash-ref data 'absolute-error)))
         'invalid] ; HACK: should specify if invalid or unsamplable
        [else
         (define repr (repr-of expr ctx))
         (define total-bits (representation-total-bits repr))
         (define bits-error (ulps->bits (first (hash-ref data 'ulp-errs))))
         (* 100 (- 1 (/ bits-error total-bits)))]))
    (hasheq 'ulps-error
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
              [_ abs-error])))

  (expr->json-tree expr ctx expr-data))
