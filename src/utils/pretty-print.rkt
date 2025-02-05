#lang racket

(require math/bigfloat)
(provide bigfloat-interval-shortest
         bigfloat-pick-point)

(define (bigfloat->normal-string x)
  (cond
    [(bfzero? x)
     (match (bigfloat-signbit x)
       [0 '+zero]
       [1 '-zero])]
    [(bfinfinite? x)
     (match (bigfloat-signbit x)
       [0 '+inf]
       [1 '-inf])]
    [(bfnan? x) 'nan]
    [else
     (define s (bigfloat->string x))
     (define-values (sign s-abs)
       (if (string-prefix? s "-")
           (values '- (substring s 1))
           (values '+ s)))
     (define-values (mantissa e)
       (match (string-split s-abs "e" #:trim? #f)
         [(list m e) (values m (string->number e))]
         [(list m) (values m 0)]))
     (define-values (pre-dot post-dot e*)
       (match (string-split mantissa "." #:trim? #f)
         [(list pre)
          (if (= (string-length pre) 1)
              (values pre "0" e)
              (values (substring pre 0 1) (substring pre 1) (- (string-length pre) 1)))]
         [(list "0" s)
          (let loop ([idx 0]
                     [e e])
            (if (eq? (string-ref s idx) #\0)
                (loop (+ idx 1) (- e 1))
                (values (substring s idx (+ idx 1)) (substring s (+ idx 1)) (- e 1))))]
         [(list pre post)
          (if (= (string-length pre) 1)
              (values pre post e)
              (values (substring pre 0 1)
                      (string-append (substring pre 1) post)
                      (+ (string-length pre) -1 e)))]))
     (list sign (string->number pre-dot) post-dot e*)]))

(module+ test
  (require rackunit)

  (check-equal? (bigfloat->normal-string (bf -1e-100))
                '(- 1 "000000000000000019991899802602883619648" -100))
  (check-equal? (bigfloat->normal-string (bf "-1")) '(- 1 "0" 0))
  (check-equal? (bigfloat->normal-string (bf "-.1"))
                '(- 1 "000000000000000000000000000000000000001" -1))
  (check-equal? (bigfloat->normal-string (bf "-.000001"))
                '(- 1 "000000000000000000000000000000000000001" -6))
  (check-equal? (bigfloat->normal-string (bf "0")) '+zero)
  (check-equal? (bigfloat->normal-string (bf "-1964363925810.15"))
                '(- 1 "964363925810149999999999999999999999999" 12))
  (check-equal? (bigfloat->normal-string (bf "-3.1014574914586375e-17"))
                '(- 3 "101457491458637500000000000000000000006" -17)))

(define (digit-interval-shortest a b)
  (define digits '(0 5 2 4 6 8 1 3 7 9))
  (for/first ([d digits]
              #:when (<= a d b))
    d))

(define (string-interval-shortest a b)
  (let loop ([idx 0])
    (cond
      [(>= idx (string-length a)) a]
      [(eq? (string-ref b idx) (string-ref a idx)) (loop (+ idx 1))]
      [else
       (format "~a~a~a"
               (substring b 0 idx)
               (let ([x-digit (string->number (substring a idx (+ idx 1)))]
                     [y-digit (string->number (substring b idx (+ idx 1)))])
                 (digit-interval-shortest (+ x-digit 1) y-digit))
               (build-string (- (string-length b) idx 1) (const #\0)))])))

(define (string-pad s n c)
  (define k (- n (string-length s)))
  (if (> k 0)
      (string-append (build-string k (const c)) s)
      s))

(module+ main
  (require rackunit)
  (check string=? (string-pad "1" 2 #\0) "01"))

(define (integer-interval-shortest a b)
  (define sa (number->string a))
  (define sb (number->string b))
  (cond
    [(<= a 0 b) 0]
    [(< b 0) (- (integer-interval-shortest (- b) (- a)))]
    [else
     (define s1 (string-pad sa (max (string-length sa) (string-length sb)) #\0))
     (define s2 (string-pad sb (max (string-length sa) (string-length sb)) #\0))
     (string->number (string-interval-shortest s1 s2))]))

(define/contract (bigfloat-interval-shortest x y)
  (->i ([x bigfloat?] [y bigfloat?]) #:pre (x y) (or (bf<= x y) (bfnan? y)) [result bigfloat?])
  (define x-parts (bigfloat->normal-string x))
  (define y-parts (bigfloat->normal-string y))
  (cond
    [(bf= x y) y]
    [(symbol? x-parts) x]
    [(symbol? y-parts) y]
    [(bfnegative? y) (bf- (bigfloat-interval-shortest (bf- y) (bf- x)))]
    [(bfnegative? x) 0.bf]
    [else
     (match-define (list x-sign x-pre x-post x-e) x-parts)
     (match-define (list y-sign y-pre y-post y-e) y-parts)
     (cond
       [(>= y-e (+ x-e 1)) (bf (format "1.0e~a" (integer-interval-shortest (+ x-e 1) y-e)))]
       [(>= y-pre (+ x-pre 1))
        (bf (format "~a.0e~a" (digit-interval-shortest (+ x-pre 1) y-pre) y-e))]
       [else (bf (format "~a.~ae~a" y-pre (string-interval-shortest x-post y-post) y-e))])]))

;; a little more rigorous than it sounds:
;; finds the shortest number `x` near `p1` such that
;; `x1` is in `[p1, p2]` and is no larger than
;;  - if `p1` is negative, `p1 / 2`
;;  - if `p1` is positive, `p1 * 2`
(define/contract (bigfloat-pick-point left right)
  (->i ([x bigfloat?] [y bigfloat?]) #:pre (x y) (or (bf<= x y) (bfnan? y)) [result bigfloat?])
  (cond
    [(and (bfnegative? left) (bfnegative? right)) (bf- (bigfloat-pick-point (bf- right) (bf- left)))]
    [(and (bfpositive? left) (bfpositive? right))
     (bigfloat-interval-shortest left (bfmin (bf* left 2.bf) right))]
    [else (bigfloat-interval-shortest left right)]))

(module+ test
  (require math/base)

  (define (sample-bigfloat)
    (define exponent (random -1023 1023)) ; Pretend-double
    (define significand (bf (random-bits (bf-precision)) (- (bf-precision))))
    (define val (bfshift (bf+ 1.bf significand) exponent))
    (if (= (random 0 2) 1)
        (bf- val)
        val))

  (for ([i (in-range 10000)])
    (define x (sample-bigfloat))
    (define y (sample-bigfloat))
    (define-values (x* y*)
      (if (bf< x y)
          (values x y)
          (values y x)))
    (define z (bigfloat-interval-shortest x* y*))
    (with-check-info (['x x*] ['z z] ['y y*]) (check bf<= x* z) (check bf<= z y*))))
