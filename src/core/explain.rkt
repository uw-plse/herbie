#lang racket

(require racket/set
         math/bigfloat
         racket/hash)
(require "points.rkt"
         "../syntax/types.rkt"
         "localize.rkt"
         "../utils/common.rkt"
         "sampling.rkt"
         "../syntax/sugar.rkt"
         "programs.rkt"
         "../utils/float.rkt"
         "../config.rkt"
         "logfloat.rkt"
         "compiler.rkt"
         "dd.rkt")

(provide explain)

(define *top-3* (make-parameter #f))

(define (take-top-n lst)
  (if (*top-3*)
      (take-n 3 lst)
      lst))

(define (take-n n lst)
  (match lst
    ['() '()]
    [(cons x xs)
     (if (= n 0)
         '()
         (cons x (take-n (- n 1) xs)))]))

(define (constant? expr)
  (cond
    [(list? expr) (andmap constant? (rest expr))]
    [(symbol? expr) #f]
    [else #t]))

(define condthres.dl (lf 100.0))
(define maybethres.dl (lf 32.0))

(define (actual-errors expr pcontext)

  (define errs
    (parameterize ([*pcontext* pcontext])
      (first (compute-local-errors (list (all-subexpressions expr)) (*context*)))))

  (define pruned (make-hash))
  (for ([(k v) (in-hash errs)])
    (hash-set! pruned k (hash-ref v 'errs)))
  (define idk (flip-lists (hash->list pruned)))
  (match-define (cons subexprs pt-errorss) idk)

  (define pt-worst-subexpr
    (append* (reap [sow]
                   (for ([pt-errors (in-list pt-errorss)]
                         [(pt _) (in-pcontext pcontext)])
                     (define sub-error (map cons subexprs pt-errors))
                     (define filtered-sub-error (filter (lambda (p) (> (cdr p) 16)) sub-error))
                     (define mapped-sub-error (map (lambda (p) (cons (car p) pt)) filtered-sub-error))
                     (unless (empty? mapped-sub-error)
                       (sow mapped-sub-error))))))

  (for/hash ([group (in-list (group-by car pt-worst-subexpr))])
    (let ([key (caar group)]) (values key (map cdr group)))))

(define (same-sign? a b)
  (or (and (bfpositive? a) (bfpositive? b)) (and (bfnegative? a) (bfnegative? b))))

(define all-explanations (list 'uflow-rescue 'u/u 'u/n 'o/o 'n/o 'o*u 'u*o 'n*u 'cancellation))
(define cond-thres (bf 100))
(define maybe-cond-thres (bf 32))

(define (compile-expr expr ctx)
  (define subexprs (all-subexpressions expr #:reverse? #t))
  (define spec-list (map prog->spec subexprs))
  (define ctxs
    (for/list ([subexpr (in-list subexprs)])
      (struct-copy context ctx [repr (repr-of subexpr ctx)])))

  (define repr-hash
    (make-immutable-hash (map (lambda (e ctx) (cons e (context-repr ctx))) subexprs ctxs)))

  (define subexprs-fn
    (parameterize ([*max-mpfr-prec* 128])
      (eval-progs-real spec-list ctxs)))

  (define subexprs-lf (compile-lfs (map expr->lf subexprs) ctx))

  (values subexprs repr-hash subexprs-fn subexprs-lf))

(define (predict-errors ctx pctx subexprs-list repr-hash subexprs-fn subexprs-lf)
  (define error-count-hash (make-hash (map (lambda (x) (cons x '())) subexprs-list)))
  (define uflow-hash (make-hash))
  (define oflow-hash (make-hash))

  (define expls->points (make-hash))
  (define maybe-expls->points (make-hash))

  (for ([(pt _) (in-pcontext pctx)])
    (define (silence expr)
      (define subexprs (all-subexpressions expr #:reverse? #t))
      (for* ([subexpr (in-list subexprs)]
             #:when (list? subexpr)
             [expl (in-list all-explanations)])
        (define key (cons subexpr expl))
        (when (hash-has-key? expls->points key)
          (hash-update! expls->points key (lambda (x) (set-remove x pt))))
        (when (hash-has-key? maybe-expls->points key)
          (hash-update! maybe-expls->points key (lambda (x) (set-remove x pt))))))

    (define (mark-erroneous! expr expl)
      (hash-update! error-count-hash expr (lambda (x) (set-add x pt)))
      (hash-update! expls->points (cons expr expl) (lambda (x) (set-add x pt)) '()))

    (define (mark-maybe! expr [expl 'sensitivity])
      (hash-update! maybe-expls->points (cons expr expl) (lambda (x) (set-add x pt)) '()))

    (define exacts (apply subexprs-fn pt))
    (define exacts-hash (make-immutable-hash (map cons subexprs-list exacts)))
    (define (exacts-ref subexpr)
      (define exacts-val (hash-ref exacts-hash subexpr))
      ((representation-repr->bf (hash-ref repr-hash subexpr)) exacts-val))

    (define lfs
      (vector-map (lambda (x)
                    (if (logfloat? x)
                        (lf-normalize x)
                        x))
                  (apply subexprs-lf (map lf pt))))
    (define lfs-hash (make-immutable-hash (map cons subexprs-list (vector->list lfs))))
    (define (lfs-ref subexpr)
      (hash-ref lfs-hash subexpr))

    (for/list ([subexpr (in-list subexprs-list)])
      (define subexpr-val (exacts-ref subexpr))
      (define z.dl (lfs-ref subexpr))

      (define (update-flow-hash flow-hash pred? . children)
        (define child-set
          (foldl (lambda (a b) (hash-union a b #:combine +))
                 (make-immutable-hash)
                 (map (lambda (a) (hash-ref flow-hash a (make-immutable-hash))) children)))
        (define parent-set (hash-ref flow-hash subexpr (make-immutable-hash)))
        (define parent+child-set (hash-union parent-set child-set #:combine (lambda (_ v) v)))
        (define new-parent-set
          (if (and (bigfloat? subexpr-val) (pred? subexpr-val))
              (hash-update parent+child-set subexpr (lambda (x) (+ x 1)) 0)
              parent+child-set))
        (hash-set! flow-hash subexpr new-parent-set))

      (match subexpr
        [(list _ x-ex y-ex z-ex)
         (update-flow-hash oflow-hash bfinfinite? x-ex y-ex z-ex)
         (update-flow-hash uflow-hash bfzero? x-ex y-ex z-ex)]
        [(list _ x-ex y-ex)
         (update-flow-hash oflow-hash bfinfinite? x-ex y-ex)
         (update-flow-hash uflow-hash bfzero? x-ex y-ex)]
        [(list _ x-ex)
         (update-flow-hash oflow-hash bfinfinite? x-ex)
         (update-flow-hash uflow-hash bfzero? x-ex)]
        [_ #f])

      (when (list? subexpr)
        (let* ([op (first subexpr)]
               [op (symbol->string op)]
               [f32? (string-suffix? op ".f32")])

          (lf-precision (if f32? 32 64))))

      (match subexpr
        [(list (or '+.f64 '+.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))

         (define x.dl (lfs-ref x-ex))
         (define y.dl (lfs-ref y-ex))
         (define condx.dl (lfabs (lf/ x.dl z.dl)))
         (define condy.dl (lfabs (lf/ x.dl z.dl)))

         (define x.eps (+ 127 (bigfloat-exponent x)))
         (define y.eps (+ 127 (bigfloat-exponent y)))

         (match-define (logfloat _ _ _ xe1 xe2) x.dl)
         (match-define (logfloat _ _ _ ye1 ye2) y.dl)

         (cond
           [(dd> (dd- xe1 xe2 ye1 ye2) 100) (silence y-ex)]
           [(dd> (dd- ye1 ye2 xe1 xe2) 100) (silence x-ex)])

         (if (or (lfover/underflowed? x.dl) (lfover/underflowed? y.dl))
             (cond
               ; NaN rescue: +-inf + -+inf cannot be nan
               [(and (lfoverflow? x.dl) (lfoverflow? y.dl) (not (lfsamesign? x.dl y.dl)))
                (mark-erroneous! subexpr 'nan-rescue)]

               ; inf rescue: inf + val can become a val'
               [(and (lfoverflow? x.dl) (not (lfoverflow? z.dl)))
                (mark-erroneous! subexpr 'oflow-left)]
               [(and (lfoverflow? y.dl) (not (lfoverflow? z.dl)))
                (mark-erroneous! subexpr 'oflow-right)])

             (cond
               ; G+x = x / x + y, G+y = y / x + y
               [(or (lf> condx.dl condthres.dl #f) (lf> condy.dl condthres.dl #f))
                (mark-erroneous! subexpr 'cancellation)]

               [(or (lf> condx.dl maybethres.dl #f) (lf> condy.dl maybethres.dl #f))
                (mark-maybe! subexpr 'cancellation)]))]

        [(list (or '-.f64 '-.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))

         (define x.dl (lfs-ref x-ex))
         (define y.dl (lfs-ref y-ex))
         (define condx.dl (lfabs (lf/ x.dl z.dl)))
         (define condy.dl (lfabs (lf/ x.dl z.dl)))

         (define x.eps (+ 127 (bigfloat-exponent x)))
         (define y.eps (+ 127 (bigfloat-exponent y)))

         (match-define (logfloat _ _ _ xe1 xe2) x.dl)
         (match-define (logfloat _ _ _ ye1 ye2) y.dl)

         (eprintf "~a ~a ~a ~a ~a ~a\n" x.eps y.eps xe1 xe2 ye1 ye2)

         (cond
           [(dd> (dd- xe1 xe2 ye1 ye2) 100) (silence y-ex)]
           [(dd> (dd- ye1 ye2 xe1 xe2) 100) (silence x-ex)])

         #;(cond
             [(> (- x.eps y.eps) 100) (silence y-ex)]
             [(> (- y.eps x.eps) 100) (silence x-ex)])

         (if (or (lfover/underflowed? x.dl) (lfover/underflowed? y.dl))
             (cond
               ; NaN rescue: +-inf - -+inf cannot be nan
               [(and (lfoverflow? x.dl) (lfoverflow? y.dl) (lfsamesign? x.dl y.dl))
                (mark-erroneous! subexpr 'nan-rescue)]

               ; inf rescue: inf - val can become a val'
               [(and (lfoverflow? x.dl) (not (lfoverflow? z.dl)))
                (mark-erroneous! subexpr 'oflow-left)]
               [(and (lfoverflow? y.dl) (not (lfoverflow? z.dl)))
                (mark-erroneous! subexpr 'oflow-right)])

             (cond
               ; G+x = x / x - y, G+y = y / x - y
               [(or (lf> condx.dl condthres.dl #f) (lf> condy.dl condthres.dl #f))
                (mark-erroneous! subexpr 'cancellation)]

               [(or (lf> condx.dl maybethres.dl #f) (lf> condy.dl maybethres.dl #f))
                (mark-maybe! subexpr 'cancellation)]))]

        [(list (or 'sin.f64 'sin.f32) x-ex)
         #:when (list? x-ex)
         (define x.lf (lfs-ref x-ex))
         (match-define (logfloat x r_x _ _ _) x.lf)
         (define-values (z r_z) (ddrsin x 0.0))
         (define-values (cond1 cond2)
           (let*-values ([(t1 t2) (ddrtan x r_x)]
                         [(c1 c2) (dd/ 1.0 0.0 t1 t2)]
                         [(c1 c2) (dd* x r_x c1 c2)])
             (ddabs c1 c2)))
         (define re (abs (/ r_x x)))
         (define mu (abs (/ r_z z)))

         (define-values (ae1 ae2) (dd* cond1 cond2 re))

         (define cot.lf (lfabs (lf/ (lf 1.0) (lftan x.lf))))
         (define cond.lf (lf* (lfabs x.lf) cot.lf))

         ;(match-define (logfloat er1 er2 _ _ _) (lf* eps cond.lf))

         ;(eprintf "~a ~a ~a ~a ~a ~a\n" pt eps cond.lf er1 er2 s)

         (if (lfover/underflowed? x.lf)
             (when (lfoverflow? x.lf)
               (mark-erroneous! subexpr 'oflow-rescue))

             (cond
               [(and (lf> cond.lf condthres.dl #f) (lf> (lfabs x.lf) condthres.dl #f))
                (mark-erroneous! subexpr 'sensitivity)]

               [(and (lf> cond.lf condthres.dl #f) (lf> cot.lf condthres.dl #f))
                (mark-erroneous! subexpr 'cancelation)]

               [(and (lf> cond.lf maybethres.dl #f) (lf> (lfabs x.lf) maybethres.dl #f))
                (mark-maybe! subexpr 'sensitivity)]

               [(and (lf> cond.lf maybethres.dl #f) (lf> cot.lf maybethres.dl #f))
                (mark-maybe! subexpr 'cancellation)]

               [else #f]))]

        [(list (or 'cos.f64 'cos.f32) x-ex)
         #:when (list? x-ex)
         (define x.lf (lfs-ref x-ex))
         (define tan.lf (lfabs (lftan x.lf)))
         (define cond.lf (lf* (lfabs x.lf) tan.lf))

         (if (lfover/underflowed? x.lf)
             (when (lfoverflow? x.lf)
               (mark-erroneous! subexpr 'sensitivity))

             (cond
               [(and (lf> cond.lf condthres.dl #f) (lf> (lfabs x.lf) condthres.dl #f))
                (mark-erroneous! subexpr 'sensitivity)]

               [(and (lf> cond.lf condthres.dl #f) (lf> tan.lf condthres.dl #f))
                (mark-erroneous! subexpr 'cancelation)]

               [(and (lf> cond.lf maybethres.dl #f) (lf> (lfabs x.lf) maybethres.dl #f))
                (mark-maybe! subexpr 'sensitivity)]

               [(and (lf> cond.lf maybethres.dl #f) (lf> tan.lf maybethres.dl #f))
                (mark-maybe! subexpr 'cancellation)]

               [else #f]))]

        [(list (or 'cos.f64 'cos.f32) x-ex)
         #:when (list? x-ex)
         (define x.lf (lfs-ref x-ex))
         (define tan.lf (lfabs (lftan x.lf)))
         (define cond.lf (lf* (lfabs x.lf) tan.lf))

         (if (lfover/underflowed? x.lf)
             (when (lfoverflow? x.lf)
               (mark-erroneous! subexpr 'sensitivity))

             (cond
               [(and (lf> cond.lf condthres.dl #f) (lf> (lfabs x.lf) condthres.dl #f))
                (mark-erroneous! subexpr 'sensitivity)]

               [(and (lf> cond.lf condthres.dl #f) (lf> tan.lf condthres.dl #f))
                (mark-erroneous! subexpr 'cancelation)]

               [(and (lf> cond.lf maybethres.dl #f) (lf> (lfabs x.lf) maybethres.dl #f))
                (mark-maybe! subexpr 'sensitivity)]

               [(and (lf> cond.lf maybethres.dl #f) (lf> tan.lf maybethres.dl #f))
                (mark-maybe! subexpr 'cancellation)]

               [else #f]))]

        [(list (or 'tan.f64 'tan.f32) x-ex)
         #:when (list? x-ex)
         (define x.lf (lfs-ref x-ex))
         (define tan.lf (lftan x.lf))
         (define cot.lf (lf/ (lf 1.0) tan.lf))
         (define condhlf.lf (lfabs (lf+ tan.lf cot.lf)))
         (define cond.lf (lf* (lfabs x.lf) condhlf.lf))

         (if (lfover/underflowed? x.lf)
             (when (lfoverflow? x.lf)
               (mark-erroneous! subexpr 'sensitivity))

             (cond
               [(and (lf> cond.lf condthres.dl #f) (lf> (lfabs x.lf) condthres.dl #f))
                (mark-erroneous! subexpr 'sensitivity)]

               [(and (lf> cond.lf condthres.dl #f) (lf> condhlf.lf condthres.dl #f))
                (mark-erroneous! subexpr 'cancelation)]

               [(and (lf> cond.lf maybethres.dl #f) (lf> (lfabs x.lf) maybethres.dl #f))
                (mark-maybe! subexpr 'sensitivity)]

               [(and (lf> cond.lf maybethres.dl #f) (lf> condhlf.lf maybethres.dl))
                (mark-maybe! subexpr 'cancellation)]

               [else #f]))]

        [(list (or 'sqrt.f64 'sqrt.f32) x-ex)
         #:when (list? x-ex)
         (define x.lf (lfs-ref x-ex))
         (when (lf< x.lf (lf 0.0) #f)
           (eprintf "~a, ~a, ~a\n" subexpr pt x.lf))

         (cond
           ;; Underflow rescue:
           [(and (lfunderflow? x.lf) (not (lfunderflow? z.dl)))
            (mark-erroneous! subexpr 'uflow-rescue)]

           ;; Overflow rescue:
           [(and (lfoverflow? x.lf) (not (lfoverflow? z.dl)))
            (mark-erroneous! subexpr 'oflow-rescue)])]

        [(list (or 'cbrt.f64 'cbrt.f32) x-ex)
         #:when (list? x-ex)
         (define x.lf (lfs-ref x-ex))

         (cond
           ;; Underflow rescue:
           [(and (lfunderflow? x.lf) (not (lfunderflow? z.dl)))
            (mark-erroneous! subexpr 'uflow-rescue)]

           ;; Overflow rescue:
           [(and (lfoverflow? x.lf) (not (lfoverflow? z.dl)))
            (mark-erroneous! subexpr 'oflow-rescue)])]

        [(list (or '/.f64 '/.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x.lf (lfs-ref x-ex))
         (define y.lf (lfs-ref y-ex))

         (when (or (lfover/underflowed? x.lf) (lfover/underflowed? y.lf))
           (cond
             ;; if the numerator underflows and the denominator:
             ;; - underflows, nan could be rescued
             [(and (lfunderflow? x.lf) (lfunderflow? y.lf) (not (lfnan? z.dl)))
              (mark-erroneous! subexpr 'u/u)]
             ;; - is small enough, 0 underflow could be rescued
             [(and (lfunderflow? x.lf) (not (lfunderflow? z.dl))) (mark-erroneous! subexpr 'u/n)]
             ;; - overflows, no rescue is possible

             ;; if the numerator overflows and the denominator:
             ;; - overflows, nan could be rescued
             [(and (lfoverflow? x.lf) (lfoverflow? y.lf) (not (lfnan? z.dl)))
              (mark-erroneous! subexpr 'o/o)]
             ;; - is large enough, inf overflow can be rescued
             [(and (lfoverflow? x.lf) (not (lfoverflow? z.dl))) (mark-erroneous! subexpr 'o/n)]
             ;; - underflow, no rescue is possible

             ;; if the numerator is normal and the denominator:
             ;; - overflows, then a rescue is possible
             [(and (lfoverflow? y.lf) (not (lfunderflow? z.dl))) (mark-erroneous! subexpr 'n/o)]
             ;; - underflows, then a rescue is possible
             [(and (lfunderflow? y.lf) (not (lfoverflow? z.dl))) (mark-erroneous! subexpr 'n/u)]
             ;; - is normal, then no rescue is possible
             [else #f]))]

        [(list (or '*.f64 '*.f32) x-ex y-ex)
         #:when (or (list? x-ex) [list? y-ex])
         (define x.lf (lfs-ref x-ex))
         (define y.lf (lfs-ref y-ex))

         (cond
           ;; if one operand underflows and the other overflows, then nan must
           ;; be rescued.
           [(and (lfoverflow? x.lf) (lfunderflow? y.lf) (not (lfnan? z.dl)))
            (mark-erroneous! subexpr 'o*u)]
           [(and (lfunderflow? x.lf) (lfoverflow? y.lf) (not (lfnan? z.dl)))
            (mark-erroneous! subexpr 'u*o)]

           ;; If one operand is normal and the other overflows then, inf rescue
           ;; could occur
           [(and (or (lfoverflow? x.lf) (lfoverflow? y.lf)) (not (lfoverflow? z.dl)))
            (mark-erroneous! subexpr 'n*o)]

           [(and (or (lfunderflow? x.lf) (lfunderflow? y.lf)) (not (lfunderflow? z.dl)))
            (mark-erroneous! subexpr 'n*u)]

           ;; If both normal then no error
           [else #f])]

        [(list (or 'log.f64 'log.f32) x-ex)
         #:when (list? x-ex)
         (define x.lf (lfs-ref x-ex))
         (define cond.lf (lfabs (lf/ (lf 1.0) z.dl)))

         (if (lfover/underflowed? x.lf)
             (cond
               [(lfunderflow? x.lf) (mark-erroneous! subexpr 'uflow-rescue)]
               [(lfoverflow? x.lf) (mark-erroneous! subexpr 'oflow-rescue)])
             (cond
               [(lf> cond.lf condthres.dl #f) (mark-erroneous! subexpr 'sensitivity)]
               [(lf> cond.lf maybethres.dl #f) (mark-maybe! subexpr 'sensitivity)]))]

        [(list (or 'exp.f64 'exp.f32) x-ex)
         #:when (list? x-ex)
         (define x.lf (lfs-ref x-ex))

         (cond
           ; Condition Number Hallucination:
           ; When x is large enough that exp(x) overflows,
           ; condition number is also high.
           ; Condition Number Hallucination:
           ; When x is large enough (negative) that exp(x)
           ; underflows, condition number is also high
           [(not (lfrepresentable? z.dl)) #f]

           ; High Condition Number:
           ; CN(exp, x) = |x|
           [(lf> (lfabs x.lf) condthres.dl #f) (mark-erroneous! subexpr 'sensitivity)]

           [(lf> (lfabs x.lf) maybethres.dl #f) (mark-maybe! subexpr 'sensitivity)]

           [else #f])]

        ; FIXME need to rework from scratch
        [(list (or 'pow.f64 'pow.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x.lf (lfs-ref x-ex))
         (define y.lf (lfs-ref y-ex))
         (define condx.lf (lfabs y.lf))
         (define condy.lf (lfabs (lf* y.lf (lflog x.lf))))

         (cond
           ;; Hallucination:
           ;; x has a large exponent and y is 1. The ylogx is large but there is
           ;; no error because the answer is exactly x
           ;; [(and (bf= y 1.bf)
           ;; (bf= x subexpr-val)) #f]

           ;; Hallucination:
           ;; y is large but x is exactly 1
           ;; [(and (= (bigfloat->flonum x) 1.0)
           ;; (= (bigfloat->flonum subexpr-val) 1.0))
           ;; #f]

           ;; Hallucination:
           ;; y is large but x is zero
           [(lfzero? x.lf) #f]

           ;; Hallucination:
           ;; if x is large enough that x^y overflows, the condition number also
           ;; is very large, but the answer correctly overflows
           [(and (lf> y.lf (lf 1.0) #f) (lfover/underflowed? z.dl)) #f]

           ;; if x is small enough and y is large enough that x^y underflows,
           ;; the condition number also gets very large, but the answer
           ;; correctly underflows

           [(and (lf< y.lf (lf -1.0) #f) (lfover/underflowed? z.dl)) #f]

           [(and (lfunderflow? x.lf) (lfrepresentable? z.dl)) (mark-erroneous! subexpr 'uflow-rescue)]

           [(and (lfoverflow? x.lf) (lfrepresentable? z.dl)) (mark-erroneous! subexpr 'oflow-rescue)]

           [(and (or (lf> condx.lf condthres.dl #f) (lf> condy.lf condthres.dl #f))
                 (not (constant? y-ex)))
            (mark-erroneous! subexpr 'sensitivity)]

           [(and (or (lf> condx.lf maybethres.dl #f) (lf> condy.lf maybethres.dl #f))
                 (not (constant? y-ex)))
            (mark-maybe! subexpr 'sensitivity)]

           [else #f])]

        [(list (or 'acos.f64 'acos.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define cond-x (bfabs (bf/ x (bf* (bfsqrt (bf- 1.bf (bf* x x))) subexpr-val))))

         (cond
           ; Condition number hallucinations:
           ; acos(1) == 0
           ;; [(and (bf= x 1.bf) (bfzero? subexpr-val)) #f]

           ; acos(-1) == pi
           ;; [(bf= x -1.bf)  #f]

           ; High Condition Number:
           ; CN(acos, x) = |x / (√(1 - x^2)acos(x))|
           [(bf> cond-x cond-thres) (mark-erroneous! subexpr 'sensitivity)]

           [(bf> cond-x maybe-cond-thres) (mark-maybe! subexpr 'sensitivity)]

           [else #f])]

        [(list (or 'asin.f64 'asin.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define cond-x (bfabs (bf/ x (bf* (bfsqrt (bf- 1.bf (bf* x x))) subexpr-val))))

         (cond
           ; Condition Number hallucinations:
           ; asin(1) == pi/2
           ;; [(bf= (bfabs x) 1.bf) #f]
           ;; [(and (bfzero? x) (bfzero? subexpr-val)) #f]
           ; High Condition Number:
           ; CN(acos, x) = |x / (√(1 - x^2)asin(x))|
           [(bf> cond-x cond-thres) (mark-erroneous! subexpr 'sensitivity)]

           [(bf> cond-x maybe-cond-thres) (mark-maybe! subexpr 'sensitivity)]

           [else #f])]
        [_ #f])))
  (values error-count-hash expls->points maybe-expls->points oflow-hash uflow-hash))

(define (generate-timelines expr
                            ctx
                            pctx
                            error-count-hash
                            expls->points
                            maybe-expls->points
                            oflow-hash
                            uflow-hash)

  (define tcount-hash (actual-errors expr pctx))

  (define repr (repr-of expr (*context*)))
  (define (values->json vs repr)
    (map (lambda (value) (value->json value repr)) vs))

  (define fperrors
    (for/list ([subexpr (in-list (set-union (hash-keys tcount-hash) (hash-keys error-count-hash)))])
      (define pset (hash-ref error-count-hash subexpr '()))
      (define tset (hash-ref tcount-hash subexpr '()))
      (define opred (set-subtract pset tset))
      (define upred (set-subtract tset pset))

      (list (~a subexpr)
            (length tset)
            (length opred)
            (and (not (empty? opred)) (values->json (first opred) repr))
            (length upred)
            (and (not (empty? upred)) (values->json (first upred) repr)))))

  (define true-error-hash
    (for/hash ([(key _) (in-pcontext pctx)]
               [value (in-list (errors expr pctx ctx))])
      (values key value)))

  (define explanations-table
    (for/list ([(key val) (in-dict expls->points)]
               #:unless (zero? (length val)))
      (define expr (car key))
      (define expl (cdr key))
      (define err-count (length val))
      (define maybe-count (length (hash-ref maybe-expls->points key '())))
      (define flow-list (make-flow-table oflow-hash uflow-hash expr expl))

      (list (~a (car expr)) (~a expr) (~a expl) err-count maybe-count flow-list)))

  (define sorted-explanations-table (take-top-n (sort explanations-table > #:key fourth)))

  (define (expls-to-points expls->points)
    (define expls-points-list (hash->list expls->points))
    (define sorted-list (sort expls-points-list > #:key (lambda (x) (length (rest x)))))
    (define points-per-expl-test (map rest sorted-list))
    (define top-3 (take-top-n points-per-expl-test))
    (define points-err (apply set-union '() top-3))
    (for/hash ([point (in-list points-err)])
      (values point true)))

  (define predicted-total-error (expls-to-points expls->points))
  (define maybe-predicted-total-error (expls-to-points maybe-expls->points))

  (define confusion-matrix (calculate-confusion true-error-hash predicted-total-error pctx))

  (define maybe-confusion-matrix
    (calculate-confusion-maybe true-error-hash
                               predicted-total-error
                               maybe-predicted-total-error
                               pctx))
  (define has-any-error?
    (for/or ([(pt _) (in-pcontext pctx)])
      (> (hash-ref true-error-hash pt) 16)))
  (define predicted-any-error?
    (for/or ([(pt _) (in-pcontext pctx)])
      (hash-ref predicted-total-error pt false)))
  (define maybe-any-error?
    (for/or ([(pt _) (in-pcontext pctx)])
      (hash-ref maybe-predicted-total-error pt false)))

  (define total-confusion-matrix
    (list (if (and has-any-error? predicted-any-error?) 1 0)
          (if (and has-any-error? (not predicted-any-error?) maybe-any-error?) 1 0)
          (if (and has-any-error? (not predicted-any-error?) (not maybe-any-error?)) 1 0)
          (if (and (not has-any-error?) predicted-any-error?) 1 0)
          (if (and (not has-any-error?) (not predicted-any-error?) maybe-any-error?) 1 0)
          (if (and (not has-any-error?) (not predicted-any-error?) (not maybe-any-error?)) 1 0)))

  (define points->expl (make-hash))

  (for ([(_ points) (in-dict expls->points)])
    (for ([pt (in-list points)])
      (hash-update! points->expl pt (lambda (x) (+ 1 x)) 0)))

  (define freqs (make-hash))

  (for ([(pt _) (in-pcontext pctx)])
    (define freq (hash-ref points->expl pt 0))
    (hash-update! freqs freq (lambda (x) (+ 1 x)) 0))

  (values fperrors
          sorted-explanations-table
          confusion-matrix
          maybe-confusion-matrix
          total-confusion-matrix
          freqs))

(define (explain expr ctx pctx)
  (define-values (subexprs-list repr-hash subexprs-fn subexprs-lf) (compile-expr expr ctx))

  (define-values (error-count-hash expls->points maybe-expls->points oflow-hash uflow-hash)
    (predict-errors ctx pctx subexprs-list repr-hash subexprs-fn subexprs-lf))

  (generate-timelines expr
                      ctx
                      pctx
                      error-count-hash
                      expls->points
                      maybe-expls->points
                      oflow-hash
                      uflow-hash))

(define (flow-list flow-hash expr type)
  (for/list ([(k v) (in-dict (hash-ref flow-hash expr))])
    (list (~a k) type v)))

(define (make-flow-table oflow-hash uflow-hash expr expl)
  (match (list expl expr)
    [(list 'oflow-rescue _) (flow-list oflow-hash expr "overflow")]
    [(list 'uflow-rescue _) (flow-list uflow-hash expr "underflow")]
    [(list 'u/u (list _ num den))
     (append (flow-list uflow-hash num "underflow") (flow-list uflow-hash den "underflow"))]
    [(list 'u/n (list _ num _)) (flow-list uflow-hash num "underflow")]
    [(list 'o/o (list _ num den))
     (append (flow-list oflow-hash num "overflow") (flow-list oflow-hash den "overflow"))]
    [(list 'o/n (list _ num _)) (flow-list oflow-hash num "overflow")]
    [(list 'n/o (list _ _ den)) (flow-list oflow-hash den "overflow")]
    [(list 'n/u (list _ _ den)) (flow-list uflow-hash den "underflow")]
    [(list 'o*u (list _ left right))
     (append (flow-list oflow-hash left "overflow") (flow-list uflow-hash right "underflow"))]
    [(list 'u*o (list _ left right))
     (append (flow-list uflow-hash left "underflow") (flow-list oflow-hash right "overflow"))]
    [(list 'nan-rescue (list _ left right))
     (append (flow-list oflow-hash left "overflow") (flow-list oflow-hash right "overflow"))]
    [(list 'oflow-left (list left _)) (flow-list oflow-hash left "overflow")]
    [(list 'oflow-right (list _ right)) (flow-list oflow-hash right "overflow")]
    [_ '()]))

(define (calculate-confusion actual-error predicted-error pcontext)
  (define outcomes
    (for/list ([(pt _) (in-pcontext pcontext)])
      (define error-actual? (> (hash-ref actual-error pt) 16))
      (define error-predicted? (hash-ref predicted-error pt false))
      #;(when (and error-predicted? (not error-actual?))
          (eprintf "~a\n" pt))
      (cons error-actual? error-predicted?)))

  (define groups (group-by identity outcomes))
  (define counts
    (for/hash ([group (in-list groups)])
      (values (first group) (length group))))

  (define true-pos (hash-ref counts '(#t . #t) 0))
  (define false-pos (hash-ref counts '(#f . #t) 0))
  (define true-neg (hash-ref counts '(#f . #f) 0))
  (define false-neg (hash-ref counts '(#t . #f) 0))

  (list true-pos false-neg false-pos true-neg))

(define (calculate-confusion-maybe actual-error predicted-error maybe-error pcontext)
  (define outcomes
    (for/list ([(pt _) (in-pcontext pcontext)])
      (define error-actual? (> (hash-ref actual-error pt) 16))
      (define error-predicted? (hash-ref predicted-error pt false))
      (define maybe-error-predicted? (hash-ref maybe-error pt false))
      (cons error-actual?
            (cond
              [error-predicted? 'true]
              [maybe-error-predicted? 'maybe]
              [else 'false]))))

  (define groups (group-by identity outcomes))
  (define counts
    (for/hash ([group (in-list groups)])
      (values (first group) (length group))))

  (define true-pos (hash-ref counts '(#t . true) 0))
  (define false-pos (hash-ref counts '(#f . true) 0))
  (define false-neg (hash-ref counts '(#t . false) 0))
  (define true-neg (hash-ref counts '(#f . false) 0))
  (define true-maybe (hash-ref counts '(#t . maybe) 0))
  (define false-maybe (hash-ref counts '(#f . maybe) 0))

  (list true-pos true-maybe false-neg false-pos false-maybe true-neg))
