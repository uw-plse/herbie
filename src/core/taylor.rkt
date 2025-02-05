#lang racket

(require math/number-theory)
(require "../utils/common.rkt"
         "programs.rkt"
         "reduce.rkt"
         "../syntax/syntax.rkt"
         "batch.rkt")

(provide approximate)

(define (approximate exprs var #:transform [tform (cons identity identity)] #:iters [iters 5])
  (define exprs*
    (for/list ([expr (in-list exprs)])
      (simplify (replace-expression expr var ((car tform) var)))))

  ; maybe we want to remove zombie nodes after (not a big problem currently)
  (define batch (expand-taylor (progs->batch exprs*)))

  (define taylor-approxs (taylor var batch))
  (for/list ([root (in-vector (batch-roots batch))])
    (match-define (cons offset coeffs) (vector-ref taylor-approxs root))
    (define i 0)
    (define terms '())

    (define (next [iter 0])
      (define coeff (simplify (replace-expression (coeffs i) var ((cdr tform) var))))
      (set! i (+ i 1))
      (match coeff
        [0
         (if (< iter iters)
             (next (+ iter 1))
             (simplify (make-horner ((cdr tform) var) (reverse terms))))]
        [_
         (set! terms (cons (cons coeff (- i offset 1)) terms))
         (simplify (make-horner ((cdr tform) var) (reverse terms)))]))
    next))

;; Our Taylor expander prefers sin, cos, exp, log, neg over trig, htrig, pow, and subtraction
(define (expand-taylor input-batch)
  (batch-replace
   input-batch
   (lambda (node)
     (match node
       [(list '- ref1 ref2) `(+ ,ref1 (neg ,ref2))]
       [(list 'pow base (app deref 1/2)) `(sqrt ,base)]
       [(list 'pow base (app deref 1/3)) `(cbrt ,base)]
       [(list 'pow base (app deref 2/3)) `(cbrt (* ,base ,base))]
       [(list 'pow base power)
        #:when (exact-integer? (deref power))
        `(pow ,base ,power)]
       [(list 'pow base power) `(exp (* ,power (log ,base)))]
       [(list 'tan arg) `(/ (sin ,arg) (cos ,arg))]
       [(list 'cosh arg) `(* 1/2 (+ (exp ,arg) (/ 1 (exp ,arg))))]
       [(list 'sinh arg) `(* 1/2 (+ (exp ,arg) (/ -1 (exp ,arg))))]
       [(list 'tanh arg) `(/ (+ (exp ,arg) (neg (/ 1 (exp ,arg)))) (+ (exp ,arg) (/ 1 (exp ,arg))))]
       [(list 'asinh arg) `(log (+ ,arg (sqrt (+ (* ,arg ,arg) 1))))]
       [(list 'acosh arg) `(log (+ ,arg (sqrt (+ (* ,arg ,arg) -1))))]
       [(list 'atanh arg) `(* 1/2 (log (/ (+ 1 ,arg) (+ 1 (neg ,arg)))))]
       [_ node]))))

; Tests for expand-taylor
(module+ test
  (require rackunit)

  (define (test-expand-taylor expr)
    (define batch (progs->batch (list expr)))
    (define batch* (expand-taylor batch))
    (car (batch->progs batch*)))

  (check-equal? '(* 1/2 (log (/ (+ 1 x) (+ 1 (neg x))))) (test-expand-taylor '(atanh x)))
  (check-equal? '(log (+ x (sqrt (+ (* x x) -1)))) (test-expand-taylor '(acosh x)))
  (check-equal? '(log (+ x (sqrt (+ (* x x) 1)))) (test-expand-taylor '(asinh x)))
  (check-equal? '(/ (+ (exp x) (neg (/ 1 (exp x)))) (+ (exp x) (/ 1 (exp x))))
                (test-expand-taylor '(tanh x)))
  (check-equal? '(* 1/2 (+ (exp x) (/ -1 (exp x)))) (test-expand-taylor '(sinh x)))
  (check-equal? '(+ 1 (neg (+ 2 (neg 3)))) (test-expand-taylor '(- 1 (- 2 3))))
  (check-equal? '(* 1/2 (+ (exp x) (/ 1 (exp x)))) (test-expand-taylor '(cosh x)))
  (check-equal? '(/ (sin x) (cos x)) (test-expand-taylor '(tan x)))
  (check-equal? '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3))))))))
                (test-expand-taylor '(- 1 (cosh (tan 3)))))
  (check-equal? '(exp (* a (log x))) (test-expand-taylor '(pow x a)))
  (check-equal? '(+ x (sin a)) (test-expand-taylor '(+ x (sin a))))
  (check-equal? '(cbrt x) (test-expand-taylor '(pow x 1/3)))
  (check-equal? '(cbrt (* x x)) (test-expand-taylor '(pow x 2/3)))
  (check-equal? '(+ 100 (cbrt x)) (test-expand-taylor '(+ 100 (pow x 1/3))))
  (check-equal? `(+ 100 (cbrt (* x ,(approx 2 3))))
                (test-expand-taylor `(+ 100 (pow (* x ,(approx 2 3)) 1/3))))
  (check-equal? `(+ ,(approx 2 3) (cbrt x)) (test-expand-taylor `(+ ,(approx 2 3) (pow x 1/3))))
  (check-equal? `(+ (cbrt x) ,(approx 2 1/3)) (test-expand-taylor `(+ (pow x 1/3) ,(approx 2 1/3)))))

(define (make-horner var terms [start 0])
  (match terms
    ['() 0]
    [(list (cons c n)) `(* ,c ,(make-monomial var (- n start)))]
    [(list (cons c n) rest ...)
     `(* ,(make-monomial var (- n start)) (+ ,c ,(make-horner var rest n)))]))

(define (make-sum terms)
  (match terms
    ['() 0]
    [`(,x) x]
    [`(,x ,xs ...) `(+ ,x ,(make-sum xs))]))

(define (make-monomial var power)
  (cond
    [(equal? power 0) 1]
    [(equal? power 1) var]
    [(equal? power -1) `(/ 1 ,var)]
    [(positive? power) `(pow ,var ,power)]
    [(negative? power) `(pow ,var ,power)]))

(define/reset n-sum-to-cache (make-hash))

(define/reset log-cache (make-hash '((1 . ((1 -1 1))))))

(define/reset series-cache (make-hash))

(define (n-sum-to n k)
  (hash-ref! (n-sum-to-cache)
             (cons n k)
             (λ ()
               (cond
                 [(= k 0) (list (build-list n (const 0)))]
                 [(= n 1) (list (list k))]
                 [(= n 0) '()]
                 [else
                  (apply append
                         (for/list ([i (in-range 0 (+ k 1))])
                           (map (curry cons i) (n-sum-to (- n 1) (- k i)))))]))))

(define (taylor var expr-batch)
  "Return a pair (e, n), such that expr ~= e var^n"
  (define nodes (batch-nodes expr-batch))
  (define taylor-approxs (make-vector (batch-length expr-batch))) ; vector of approximations

  (for ([node (in-vector nodes)]
        [n (in-naturals)])
    (define approx
      (match node
        [(? (curry equal? var)) (taylor-exact 0 1)]
        [(? number?) (taylor-exact node)]
        [(? variable?) (taylor-exact node)]
        [`(,const) (taylor-exact node)]
        [`(+ ,args ...) (apply taylor-add (map (curry vector-ref taylor-approxs) args))]
        [`(neg ,arg) (taylor-negate ((curry vector-ref taylor-approxs) arg))]
        [`(* ,left ,right)
         (taylor-mult (vector-ref taylor-approxs left) (vector-ref taylor-approxs right))]
        [`(/ ,num ,den)
         #:when (equal? (vector-ref nodes num) 1)
         (taylor-invert (vector-ref taylor-approxs den))]
        [`(/ ,num ,den)
         (taylor-quotient (vector-ref taylor-approxs num) (vector-ref taylor-approxs den))]
        [`(sqrt ,arg) (taylor-sqrt var (vector-ref taylor-approxs arg))]
        [`(cbrt ,arg) (taylor-cbrt var (vector-ref taylor-approxs arg))]
        [`(exp ,arg)
         (let ([arg* (normalize-series (vector-ref taylor-approxs arg))])
           (if (positive? (car arg*))
               (taylor-exact (batch-ref expr-batch n))
               (taylor-exp (zero-series arg*))))]
        [`(sin ,arg)
         (let ([arg* (normalize-series (vector-ref taylor-approxs arg))])
           (cond
             [(positive? (car arg*)) (taylor-exact (batch-ref expr-batch n))]
             [(= (car arg*) 0)
              ; Our taylor-sin function assumes that a0 is 0,
              ; because that way it is especially simple. We correct for this here
              ; We use the identity sin (x + y) = sin x cos y + cos x sin y
              (taylor-add
               (taylor-mult (taylor-exact `(sin ,((cdr arg*) 0))) (taylor-cos (zero-series arg*)))
               (taylor-mult (taylor-exact `(cos ,((cdr arg*) 0))) (taylor-sin (zero-series arg*))))]
             [else (taylor-sin (zero-series arg*))]))]
        [`(cos ,arg)
         (let ([arg* (normalize-series (vector-ref taylor-approxs arg))])
           (cond
             [(positive? (car arg*)) (taylor-exact (batch-ref expr-batch n))]
             [(= (car arg*) 0)
              ; Our taylor-cos function assumes that a0 is 0,
              ; because that way it is especially simple. We correct for this here
              ; We use the identity cos (x + y) = cos x cos y - sin x sin y
              (taylor-add (taylor-mult (taylor-exact `(cos ,((cdr arg*) 0)))
                                       (taylor-cos (zero-series arg*)))
                          (taylor-negate (taylor-mult (taylor-exact `(sin ,((cdr arg*) 0)))
                                                      (taylor-sin (zero-series arg*)))))]
             [else (taylor-cos (zero-series arg*))]))]
        [`(log ,arg) (taylor-log var (vector-ref taylor-approxs arg))]
        [`(pow ,base ,power)
         #:when (exact-integer? (vector-ref nodes power))
         (taylor-pow (normalize-series (vector-ref taylor-approxs base)) (vector-ref nodes power))]
        [_ (taylor-exact (batch-ref expr-batch n))]))
    (vector-set! taylor-approxs n approx))
  taylor-approxs)

; A taylor series is represented by a function f : nat -> expr,
; representing the coefficients (the 1 / n! terms not included),
; and an integer offset to the exponent

(define (taylor-exact . terms)
  (define items (list->vector (map simplify terms)))
  (cons 0
        (λ (n)
          (if (<= (length terms) n)
              0
              (vector-ref items n)))))

(define (first-nonzero-exp f)
  "Returns n, where (series n) != 0, but (series n) = 0 for all smaller n"
  (let loop ([n 0])
    (if (and (equal? (f n) 0) (< n 20))
        (loop (+ n 1))
        n)))

(define (align-series . serieses)
  (if (or (<= (length serieses) 1) (apply = (map car serieses)))
      serieses
      (let ([offset* (car (argmax car serieses))])
        (for/list ([series serieses])
          (let ([offset (car series)])
            (cons offset*
                  (λ (n)
                    (if (< (+ n (- offset offset*)) 0)
                        0
                        ((cdr series) (+ n (- offset offset*)))))))))))

(define (taylor-add . terms)
  (match (apply align-series terms)
    [`((,offset . ,serieses) ...)
     (let ([hash (make-hash)])
       (cons (car offset)
             (λ (n)
               (hash-ref! hash
                          n
                          (λ ()
                            (simplify (make-sum (for/list ([series serieses])
                                                  (series n)))))))))]))

(define (taylor-negate term)
  (cons (car term) (λ (n) (simplify (list 'neg ((cdr term) n))))))

(define (taylor-mult left right)
  (cons (+ (car left) (car right))
        (let ([hash (make-hash)])
          (lambda (n)
            (hash-ref! hash
                       n
                       (λ ()
                         (simplify (make-sum (for/list ([i (range (+ n 1))])
                                               (list '* ((cdr left) i) ((cdr right) (- n i))))))))))))

(define (normalize-series series)
  "Fixes up the series to have a non-zero zeroth term,
   allowing a possibly negative offset"
  (match series
    [(cons offset coeffs)
     (let ([slack (first-nonzero-exp coeffs)])
       (cons (- offset slack) (compose coeffs (curry + slack))))]))

(define ((zero-series series) n)
  (if (< n (- (car series)))
      0
      ((cdr series) (+ n (car series)))))

(define (taylor-invert term)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match (normalize-series term)
    [(cons offset b)
     (let ([hash (make-hash)])
       (hash-set! hash 0 (simplify `(/ 1 ,(b 0))))
       (letrec ([f (λ (n)
                     (hash-ref! hash
                                n
                                (λ ()
                                  (simplify `(neg (+ ,@(for/list ([i (range n)])
                                                         `(* ,(f i) (/ ,(b (- n i)) ,(b 0))))))))))])
         (cons (- offset) f)))]))

(define (taylor-quotient num denom)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match-define (cons noff a) (normalize-series num))
  (match-define (cons doff b) (normalize-series denom))
  (define hash (make-hash))
  (hash-set! hash 0 (simplify `(/ ,(a 0) ,(b 0))))
  (define (f n)
    (hash-ref! hash
               n
               (λ ()
                 (simplify `(- (/ ,(a n) ,(b 0))
                               (+ ,@(for/list ([i (range n)])
                                      `(* ,(f i) (/ ,(b (- n i)) ,(b 0))))))))))
  (cons (- noff doff) f))

(define (modulo-series var n series)
  (match-define (cons offset coeffs) (normalize-series series))
  (define offset* (+ offset (modulo (- offset) n)))
  (define (coeffs* i)
    (match i
      [0
       (make-sum (for/list ([j (in-range (modulo offset n))])
                   `(* ,(coeffs j) (pow ,var ,(+ j (modulo (- offset) n))))))]
      [_
       #:when (< i n)
       0]
      [_ (coeffs (+ (- i n) (modulo offset n)))]))
  (cons offset* (if (= offset offset*) coeffs coeffs*)))

(define (taylor-sqrt var num)
  (match-define (cons offset* coeffs*) (modulo-series var 2 num))
  (let* ([hash (make-hash)])
    (hash-set! hash 0 (simplify `(sqrt ,(coeffs* 0))))
    (hash-set! hash 1 (simplify `(/ ,(coeffs* 1) (* 2 (sqrt ,(coeffs* 0))))))
    (letrec ([f (λ (n)
                  (hash-ref! hash
                             n
                             (λ ()
                               (simplify (cond
                                           [(even? n)
                                            `(/ (- ,(coeffs* n)
                                                   (pow ,(f (/ n 2)) 2)
                                                   (+ ,@(for/list ([k (in-naturals 1)]
                                                                   #:break (>= k (- n k)))
                                                          `(* 2 (* ,(f k) ,(f (- n k)))))))
                                                (* 2 ,(f 0)))]
                                           [(odd? n)
                                            `(/ (- ,(coeffs* n)
                                                   (+ ,@(for/list ([k (in-naturals 1)]
                                                                   #:break (>= k (- n k)))
                                                          `(* 2 (* ,(f k) ,(f (- n k)))))))
                                                (* 2 ,(f 0)))])))))])
      (cons (/ offset* 2) f))))

(define (taylor-cbrt var num)
  (match-define (cons offset* coeffs*) (modulo-series var 3 num))
  (let* ([f0 (simplify `(cbrt ,(coeffs* 0)))]
         [hash (make-hash)])
    (hash-set! hash 0 f0)
    (hash-set! hash 1 (simplify `(/ ,(coeffs* 1) (* 3 (cbrt (* ,f0 ,f0))))))
    (letrec ([f (λ (n)
                  (hash-ref! hash
                             n
                             (λ ()
                               (simplify `(/ (- ,(coeffs* n)
                                                ,@(for*/list ([terms (n-sum-to 3 n)]
                                                              #:unless (set-member? terms n))
                                                    (match-define (list a b c) terms)
                                                    `(* ,(f a) ,(f b) ,(f c))))
                                             (* 3 ,f0 ,f0))))))])
      (cons (/ offset* 3) f))))

(define (taylor-pow coeffs n)
  (match n ;; Russian peasant multiplication
    [(? negative?) (taylor-pow (taylor-invert coeffs) (- n))]
    [0 (taylor-exact 1)]
    [1 coeffs]
    [(? even?)
     (define half (taylor-pow coeffs (/ n 2)))
     (taylor-mult half half)]
    [(? odd?)
     (define half (taylor-pow coeffs (/ (- n 1) 2)))
     (taylor-mult coeffs (taylor-mult half half))]))

(define (all-partitions n options)
  (match options
    ['()
     (if (= n 0)
         '(())
         '())]
    [(cons k options*)
     (reap [sow]
           (for* ([i (in-range (/ (+ n 1) k))])
             (define head (cons i k))
             (if (= i 0)
                 (map sow (all-partitions n options*))
                 (for ([pt (all-partitions (- n (* k i)) options*)])
                   (sow (cons head pt))))))]))

(define (taylor-exp coeffs)
  (let* ([hash (make-hash)])
    (hash-set! hash 0 (simplify `(exp ,(coeffs 0))))
    (cons 0
          (λ (n)
            (hash-ref! hash
                       n
                       (λ ()
                         (define coeffs* (list->vector (map coeffs (range 1 (+ n 1)))))
                         (define nums
                           (for/list ([i (in-range 1 (+ n 1))]
                                      [coeff (in-vector coeffs*)]
                                      #:unless (equal? coeff 0))
                             i))
                         (simplify `(* (exp ,(coeffs 0))
                                       (+ ,@(for/list ([p (all-partitions n (sort nums >))])
                                              `(* ,@(for/list ([(count num) (in-dict p)])
                                                      `(/ (pow ,(vector-ref coeffs* (- num 1)) ,count)
                                                          ,(factorial count))))))))))))))

(define (taylor-sin coeffs)
  (let ([hash (make-hash)])
    (hash-set! hash 0 0)
    (cons 0
          (λ (n)
            (hash-ref! hash
                       n
                       (λ ()
                         (define coeffs* (list->vector (map coeffs (range 1 (+ n 1)))))
                         (define nums
                           (for/list ([i (in-range 1 (+ n 1))]
                                      [coeff (in-vector coeffs*)]
                                      #:unless (equal? coeff 0))
                             i))
                         (simplify `(+ ,@
                                       (for/list ([p (all-partitions n (sort nums >))])
                                         (if (= (modulo (apply + (map car p)) 2) 1)
                                             `(* ,(if (= (modulo (apply + (map car p)) 4) 1) 1 -1)
                                                 ,@(for/list ([(count num) (in-dict p)])
                                                     `(/ (pow ,(vector-ref coeffs* (- num 1)) ,count)
                                                         ,(factorial count))))
                                             0))))))))))

(define (taylor-cos coeffs)
  (let ([hash (make-hash)])
    (hash-set! hash 0 1)
    (cons 0
          (λ (n)
            (hash-ref! hash
                       n
                       (λ ()
                         (define coeffs* (list->vector (map coeffs (range 1 (+ n 1)))))
                         (define nums
                           (for/list ([i (in-range 1 (+ n 1))]
                                      [coeff (in-vector coeffs*)]
                                      #:unless (equal? coeff 0))
                             i))
                         (simplify `(+ ,@
                                       (for/list ([p (all-partitions n (sort nums >))])
                                         (if (= (modulo (apply + (map car p)) 2) 0)
                                             `(* ,(if (= (modulo (apply + (map car p)) 4) 0) 1 -1)
                                                 ,@(for/list ([(count num) (in-dict p)])
                                                     `(/ (pow ,(vector-ref coeffs* (- num 1)) ,count)
                                                         ,(factorial count))))
                                             0))))))))))

;; This is a hyper-specialized symbolic differentiator for log(f(x))

(define initial-logtable '((1 -1 1)))

(define (list-setinc l i)
  (let loop ([i i]
             [l l]
             [rest '()])
    (if (= i 0)
        (if (null? (cdr l))
            (append (reverse rest) (list (- (car l) 1) 1))
            (append (reverse rest) (list* (- (car l) 1) (+ (cadr l) 1) (cddr l))))
        (loop (- i 1) (cdr l) (cons (car l) rest)))))

(define (loggenerate table)
  (apply append
         (for/list ([term table])
           (match term
             [`(,coeff ,ps ...)
              (filter identity
                      (for/list ([i (in-naturals)]
                                 [p ps])
                        (if (zero? p)
                            #f
                            `(,(* coeff p) ,@(list-setinc ps i)))))]))))

(define (lognormalize table)
  (filter (λ (entry) (not (= (car entry) 0)))
          (for/list ([entry (group-by cdr table)])
            (cons (apply + (map car entry)) (cdar entry)))))

(define (logstep table)
  (lognormalize (loggenerate table)))

(define logbiggest 1)

(define (logcompute i)
  (hash-ref! (log-cache) i (λ () (logstep (logcompute (- i 1))))))

(define (taylor-log var arg)
  (match-define (cons shift coeffs) (normalize-series arg))
  (define hash (make-hash))
  (hash-set! hash 0 (simplify `(log ,(coeffs 0))))

  (define (series n)
    (hash-ref! hash
               n
               (λ ()
                 (let* ([tmpl (logcompute n)])
                   (simplify `(/ (+ ,@
                                    (for/list ([term tmpl])
                                      (match term
                                        [`(,coeff ,k ,ps ...)
                                         `(* ,coeff
                                             (/ (* ,@(for/list ([i (in-naturals 1)]
                                                                [p ps])
                                                       (if (= p 0)
                                                           1
                                                           `(pow (* ,(factorial i) ,(coeffs i)) ,p))))
                                                (pow ,(coeffs 0) ,(- k))))])))
                                 ,(factorial n)))))))

  (cons 0
        (λ (n)
          (if (and (= n 0) (not (zero? shift)))
              (simplify `(+ (* (neg ,shift) (log ,var)) ,(series 0)))
              (series n)))))

(module+ test
  (require rackunit
           "../syntax/types.rkt"
           "../syntax/load-plugin.rkt")
  (define batch (progs->batch (list '(pow x 1.0))))
  (set! batch (expand-taylor batch))
  (define root (vector-ref (batch-roots batch) 0))
  (check-pred exact-integer? (car (vector-ref (taylor 'x batch) root))))

(module+ test
  (define (coeffs expr #:n [n 7])
    (define batch (progs->batch (list expr)))
    (set! batch (expand-taylor batch))
    (define root (vector-ref (batch-roots batch) 0))
    (match-define fn (zero-series (vector-ref (taylor 'x batch) root)))
    (build-list n fn))
  (check-equal? (coeffs '(sin x)) '(0 1 0 -1/6 0 1/120 0))
  (check-equal? (coeffs '(sqrt (+ 1 x))) '(1 1/2 -1/8 1/16 -5/128 7/256 -21/1024))
  (check-equal? (coeffs '(cbrt (+ 1 x))) '(1 1/3 -1/9 5/81 -10/243 22/729 -154/6561))
  (check-equal? (coeffs '(sqrt x)) '((sqrt x) 0 0 0 0 0 0))
  (check-equal? (coeffs '(cbrt x)) '((cbrt x) 0 0 0 0 0 0))
  (check-equal? (coeffs '(cbrt (* x x))) '((cbrt (pow x 2)) 0 0 0 0 0 0)))
