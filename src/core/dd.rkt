#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         math/flonum)

(provide (all-defined-out))

(define qd-lib (ffi-lib "./QD/src/.libs/libqd"))

(define dde1 2.718281828459045091e+00)
(define dde2 1.445646891729250158e-16)
(define ddpi1 3.141592653589793116e+00)
(define ddpi2 1.224646799147353207e-16)

(define-ffi-definer define-qd qd-lib)

(define-syntax define-dd-binary
  (syntax-rules ()
    [(_ name ...)
     (begin
       (define-qd name (_fun _pointer _pointer _pointer -> _void)) ...)]))

(define-syntax define-dd-unary
  (syntax-rules ()
    [(_ name ...)
     (begin
       (define-qd name (_fun _pointer _pointer -> _void)) ...)]))

(define-qd c_dd_nroot (_fun _pointer _int _pointer -> _void))

(define-qd c_rem_pio2 (_fun _double _pointer -> _int))

(define-dd-binary
  c_dd_add
  c_dd_sub
  c_dd_mul
  c_dd_div
  c_dd_atan2)

(define-dd-unary
  c_dd_sqrt
  c_dd_sqr
  c_dd_abs
  c_dd_exp
  c_dd_log
  c_dd_log10
  c_dd_sin
  c_dd_cos
  c_dd_tan
  c_dd_aint
  c_dd_floor
  c_dd_ceil
  c_dd_asin
  c_dd_acos
  c_dd_atan
  c_dd_sinh
  c_dd_cosh
  c_dd_tanh
  c_dd_asinh
  c_dd_acosh
  c_dd_atanh)

(define x 2.456e+150)

(define (rempio2 x)
  (define y (make-f64vector 2))
  (define n (c_rem_pio2 x (f64vector->cpointer y)))
  (define-values (r1 r2) (apply values (f64vector->list y)))
  (values n r1 r2))

(define (nan.dd)
  (values +nan.0 +nan.0))

(define (+inf.dd)
  (values +inf.0 0.0))

(define (-inf.dd)
  (values -inf.0 0.0))

(define log2-hi 6.931471805599452862e-01)
(define log2-lo 2.319046813846299558e-17)

(define (log2.dd)
  (values log2-hi log2-lo))

(define (0.dd)
  (values 0.0 0.0))

(define (dd= x1 x2 y1 [y2 0])
  (and (= x1 y1) (= x2 y2)))

(define (dd> x1 x2 y1 [y2 0])
  (or (> x1 y1) (and (= x1 y1) (> x2 y2))))

(define (dd< x1 x2 y1 [y2 0])
  (dd> y1 y2 x1 x2))

(define (dd>= x1 x2 y1 [y2 0])
  (or (> x1 y1) (and (= x1 y1) (>= x2 y2))))

(define (dd<= x1 x2 y1 [y2 0])
  (dd>= y1 y2 x1 x2))

(define (ddzero? x1 [x2 0])
  (dd= x1 x2 0))

(define (ddinfinite? x1 [x2 0])
  (infinite? x1))

(define (ddposinf? x1 [x2 0])
  (dd= x1 x2 +inf.0))

(define (ddneginf? x1 [x2 0])
  (dd= x1 x2 -inf.0))

(define (ddfinite? x1 [x2 0])
  (nor (ddinfinite? x1 x2) (ddnan? x1 x2)))

(define (ddpositive? x1 [x2 0])
  (dd> x1 x2 0.0))

(define (ddnegative? x1 [x2 0])
  (dd< x1 x2 0.0))

(define (ddnan? x1 [x2 0])
  (nan? x1))

(define (ddsamesign? x1 x2 y1 [y2 0])
  (or (and (ddpositive? x1 x2) (ddpositive? y1 y2)) (and (ddnegative? x1 x2) (ddnegative? y1 y2))))

(define (pi.dd)
  (values 3.141592653589793116e+00 1.224646799147353207e-16))

(define (pi/2.dd)
  (let*-values ([(pi1 pi2) (pi.dd)])
    (dd/ pi1 pi2 2.0)))

(define (ddrem x1 x2 y1 [y2 0.0])
  (let*-values ([(a1 a2) (dd/ x1 x2 y1 y2)]
                [(b1 b2) (ddfloor a1 a2)]
                [(c1 c2) (dd* b1 b2 y1 y2)])
    (dd- x1 x2 c1 c2)))

(define (ddatan2 x1 x2 y1 [y2 0.0])
  (define a (list->f64vector (list x1 x2)))
  (define b (list->f64vector (list y1 y2)))
  (define c (make-f64vector 2))
  (c_dd_atan2 (f64vector->cpointer a) (f64vector->cpointer b) (f64vector->cpointer c))
  (apply values (f64vector->list c)))

(define (dd+ x1 x2 y1 [y2 0.0])
  (cond
    ; x + +-inf = +-inf
    [(and (ddfinite? x1 x2) (ddinfinite? y1 y2)) (values y1 y2)]
    ; +-inf + x = +-inf
    [(and (ddinfinite? x1 x2) (ddfinite? y1 y2)) (values x1 x1)]
    ; +-inf + +-inf = +inf
    ; +-inf + -+inf = +nan
    [(and (ddinfinite? x1 x2) (ddinfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (values x1 x2)
         (nan.dd))]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_add (f64vector->cpointer a) (f64vector->cpointer b) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (dd- x1 x2 y1 [y2 0.0])
  (cond
    ; x - +-inf = +-inf
    [(and (ddfinite? x1 x2) (ddinfinite? y1 y2)) (values (- y1) y2)]
    ; +-inf - x = +-inf
    [(and (ddinfinite? x1 x2) (ddfinite? y1 y2)) (values x1 x2)]
    ; +-inf - +-inf = +nan
    ; +-inf - -+inf = +inf
    [(and (ddinfinite? x1 x2) (ddinfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (nan.dd)
         (values x1 x2))]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_sub (f64vector->cpointer a) (f64vector->cpointer b) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (dd* x1 x2 y1 [y2 0.0])
  (cond
    ; 0 * inf = nan
    [(and (ddzero? x1 x2) (ddinfinite? y1 y2)) (nan.dd)]
    [(and (ddzero? y1 y2) (ddinfinite? x1 x2)) (nan.dd)]
    ; inf * inf = inf
    [(and (ddinfinite? x1 x2) (ddinfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (+inf.dd)
         (-inf.dd))]
    ; x * inf = inf
    [(and (ddfinite? x1 x2) (ddinfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (+inf.dd)
         (-inf.dd))]
    [(and (ddinfinite? x1 x2) (ddfinite? y1 y2))
     (if (ddsamesign? x1 x2 y1 y2)
         (+inf.dd)
         (-inf.dd))]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_mul (f64vector->cpointer a) (f64vector->cpointer b) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (dd/ x1 x2 y1 [y2 0.0])
  (cond
    [(and (ddinfinite? x1 x2) (ddinfinite? y1 y2)) (nan.dd)]
    [(and (ddzero? x1 x2) (ddzero? y1 y2)) (nan.dd)]
    [(and (ddfinite? x1 x2) (ddinfinite? y1 y2)) (0.dd)]
    [(and (ddpositive? x1 x2) (ddzero? y1 y2)) (+inf.dd)]
    [(and (ddnegative? x1 x2) (ddzero? y1 y2)) (-inf.dd)]
    [(and (ddinfinite? x1 x2) (ddsamesign? x1 x2 y1 y2)) (+inf.dd)]
    [(and (ddinfinite? x1 x2) (not (ddsamesign? x1 x2 y1 y2))) (-inf.dd)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define b (list->f64vector (list y1 y2)))
     (define c (make-f64vector 2))
     (c_dd_div (f64vector->cpointer a) (f64vector->cpointer b) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (ddsqrt x1 [x2 0])
  (cond
    [(ddposinf? x1 x2) (values x1 x2)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define c (make-f64vector 2))
     (c_dd_sqrt (f64vector->cpointer a) (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (ddcbrt x1 [x2 0])
  (cond
    [(ddinfinite? x1 x2) (+inf.dd)]
    [else
     (define a (list->f64vector (list x1 x2)))
     (define c (make-f64vector 2))
     (c_dd_nroot (f64vector->cpointer a) 3 (f64vector->cpointer c))
     (apply values (f64vector->list c))]))

(define (ddint? x1 [x2 0])
  (let*-values ([(a1 a2) (ddaint x1 x2)])
    (dd= x1 x2 a1 a2)))

(define (ddneg x1 [x2 0])
  (values (- x1) (- x2)))

(define (ddlog x1 [x2 0.0])
  (fl2log x1 x2))

(define (ddlog2 x1 [x2 0.0])
  (cond
    [(ddzero? x1 x2) (-inf.dd)]
    [(ddposinf? x1 x2) (+inf.dd)]
    [else
     (let*-values ([(a1 a2) (ddlog x1 x2)]
                   [(b1 b2) (log2.dd)])
       (dd/ a1 a2 b1 b2))]))

(define (ddexp2 x1 [x2 0])
  (let*-values ([(l1 l2) (log2.dd)]
                [(e1 e2) (dd* x1 x2 l1 l2)])
    (ddexp e1 e2)))

(define (ddexpt x1 x2 y1 [y2 0.0])
  (define isint? (ddint? y1 y2))
  (cond
    [(ddzero? y1 y2) (values 1.0 0.0)]
    [(dd>= x1 x2 0.0 0.0)
     (let*-values ([(a1 a2) (ddlog x1 x2)]
                   [(a1 a2) (dd* y1 y2 a1 a2)])
       (ddexp a1 a2))]
    [(and (ddnegative? x1 x2) (ddposinf? y1 y2)) (+inf.dd)]
    [(and (ddnegative? x1 x2) (ddneginf? y1 y2)) (-inf.dd)]
    [(and (ddneginf? x1 x2) (ddpositive? y1 y2))
     (if isint?
         (if (even? (exact-round y1))
             (+inf.dd)
             (-inf.dd))
         (+inf.dd))]
    [(and (ddneginf? x1 x2) (ddnegative? y1 y2)) (values 0.0 0.0)]
    [(ddint? y1 y2)
     ;; ignore the low part
     (let*-values ([(e1 _) (ddaint y1 y2)]
                   [(mod2) (or (and (even? y1) (even? y2))
                               (and (odd? y1) (odd? y2)))]
                   [(a1 a2) (ddabs x1 x2)]
                   [(a1 a2) (ddlog a1 a2)]
                   [(a1 a2) (dd* y1 y2 a1 a2)]
                   [(a1 a2) (ddexp a1 a2)])
       (if mod2
           (values a1 a2)
           (ddneg a1 a2)))]
    [else (nan.dd)]))

(define (ddrsin x1 [x2 0.0])
  (cond
    [(ddnan? x1 x2) (nan.dd)]
    [(ddinfinite? x1 x2) (nan.dd)]
    [else
     (let*-values ([(k1 a1 a2) (rempio2 x1)]
                   [(k2 b1 b2) (rempio2 x2)]
                   [(r1 r2) (dd+ a1 a2 b1 b2)]
                   [(k) (remainder (+ k1 k2) 4)]
                   [(s1 s2) (ddsin r1 r2)]
                   [(c1 c2) (ddcos r1 r2)])
       (match k
         [0 (values s1 s2)]
         [1 (values c1 c2)]
         [2 (ddneg s1 s2)]
         [3 (ddneg c1 c2)]))]))

(define (ddrcos x1 [x2 0.0])
  (cond
    [(ddnan? x1 x2) (nan.dd)]
    [(ddinfinite? x1 x2) (nan.dd)]
    [else
     (let*-values ([(k1 a1 a2) (rempio2 x1)]
                   [(k2 b1 b2) (rempio2 x2)]
                   [(r1 r2) (dd+ a1 a2 b1 b2)]
                   [(k) (remainder (+ k1 k2) 4)]
                   [(s1 s2) (ddsin r1 r2)]
                   [(c1 c2) (ddcos r1 r2)])
       (match k
         [0 (values c1 c2)]
         [1 (ddneg s1 s2)]
         [2 (ddneg c1 c2)]
         [3 (values s1 s2)]))]))

(define (ddrtan x1 [x2 0.0])
  (cond
    [(ddnan? x1 x2) (nan.dd)]
    [(ddinfinite? x1 x2) (nan.dd)]
    [else
     (let*-values ([(k1 a1 a2) (rempio2 x1)]
                   [(k2 b1 b2) (rempio2 x2)]
                   [(r1 r2) (dd+ a1 a2 b1 b2)]
                   [(k) (remainder (+ k1 k2) 4)]
                   [(t1 t2) (ddtan r1 r2)]
                   [(c1 c2) (dd/ 1.0 0.0 t1 t2)])
       (match k
         [0 (values t1 t2)]
         [1 (ddneg c1 c2)]
         [2 (values t1 t2)]
         [3 (ddneg c1 c2)]))]))

(define-syntax define-dd-unary-fn
  (syntax-rules ()
    [(_ [name1 name2] ...)
     (begin
       (define (name1 x1 [x2 0])
         (define a (list->f64vector (list x1 x2)))
         (define b (make-f64vector 2))
         (name2 (f64vector->cpointer a) (f64vector->cpointer b))
         (apply values (f64vector->list b))) ...)]))

(define-dd-unary-fn
  [ddabs c_dd_abs]
  [ddsin c_dd_sin]
  [ddcos c_dd_cos]
  [ddtan c_dd_tan]
  [ddaint c_dd_aint]
  [ddexp c_dd_exp]
  [ddfloor c_dd_floor]
  [ddceil c_dd_ceil]
  [ddasin c_dd_asin]
  [ddacos c_dd_acos]
  [ddatan c_dd_atan]
  [ddsinh c_dd_sinh]
  [ddcosh c_dd_cosh]
  [ddtanh c_dd_tanh]
  [ddasinh c_dd_asinh]
  [ddacosh c_dd_acosh]
  [ddatanh c_dd_atanh])
