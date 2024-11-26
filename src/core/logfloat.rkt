#lang racket

(require math/base
         math/flonum
         racket/struct
         "../syntax/syntax.rkt"
         "dd.rkt")

(provide (all-defined-out))

(define lf-precision (make-parameter 64))

(define (xnor a b)
  (not (xor a b)))

(struct logfloat (r1 r2 s e1 e2)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer (lambda (_) 'logfloat)
                                     (lambda (obj)
                                       (list (logfloat-r1 obj)
                                             (logfloat-r2 obj)
                                             (logfloat-s obj)
                                             (logfloat-e1 obj)
                                             (logfloat-e2 obj)))))])

(define/contract (lf x1 [x2 0.0])
  (->* (flonum?) (flonum?) logfloat?)
  (let*-values ([(e1 e2) (ddabs x1 x2)]
                [(e1 e2) (ddlog2 e1 e2)])
    (logfloat x1 x2 (dd>= x1 x2 0.0 0.0) e1 e2)))

(define (max.lf) (if (= (lf-precision) 64)
                     (logfloat +max.0 0.0 #true 1024.0 0.0)
                     (logfloat 3.40282347E+38 0.0 #true 128.0 0.0)))

(define (min.lf) (if (= (lf-precision) 64)
                     (logfloat +min.0 0.0 #true -1074.0 0.0)
                     (logfloat 1.17549435e-38 0.0 #true -126.0 0.0)))

(define +max.lf (logfloat +max.0 0.0 #true 1024.0 0.0))
(define +min.lf (logfloat +min.0 0.0 #true -1074.0 0.0))

(define/contract (lf-normalize x)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s e1 e2) x)
  (define-values (l1 l2) (ddexp2 e1 e2))
  (define-values (n1 n2) (ddneg l1 l2))
  (define-values (ne1 ne2) (ddlog2 x1 x2))
  (define ns (dd>= x1 x2 0.0))
  (cond
    [(or (ddnan? x1 x2) (ddzero? x1 x2) (ddinfinite? x1 x2))
     (if s
         (logfloat l1 l2 s e1 e2)
         (logfloat n1 n2 s e1 e2))]
    [(or (ddnan? e1 e2) (ddzero? e1 e2) (ddinfinite? e1 e2))
     (logfloat x1 x2 ns ne1 ne2)]
    [else x]))

(define/contract (lfzero? x)
  (-> logfloat? boolean?)
  (match-define (logfloat x1 x2 _ e1 e2) x)
  (and (ddzero? x1 x2) (ddinfinite? e1 e2)))

(define/contract (lf= x y [flag #t])
  (->* (logfloat? logfloat?) (boolean?) boolean?)
  (match-define (logfloat x1 x2 xs ex1 ex2) x)
  (match-define (logfloat y1 y2 ys ey1 ey2) y)
  (if flag
      (and (eq? xs ys) (dd= ex1 ex2 ey1 ey2))
      (dd= x1 x2 y1 y2)))

(define/contract (lf> x y [flag #t])
  (->* (logfloat? logfloat?) (boolean?) boolean?)
  (match-define (logfloat x1 x2 xs ex1 ex2) x)
  (match-define (logfloat y1 y2 ys ey1 ey2) y)
  (if flag
      (cond
        ; Both +ve
        [(and xs ys) (dd> ex1 ex2 ey1 ey2)]

        ; Both -ve
        [(nor xs ys) (dd> ey1 ey2 ex1 ex2)]

        [else xs])
      (dd> x1 x2 y1 y2)))

(define/contract (lf< x y [flag #t])
  (->* (logfloat? logfloat?) (boolean?) boolean?)
  (lf> y x flag))

(define/contract (lf>= x y [flag #t])
  (->* (logfloat? logfloat?) (boolean?) boolean?)
  (or (lf> x y flag) (lf= x y flag)))

(define/contract (lf<= x y [flag #t])
  (->* (logfloat? logfloat?) (boolean?) boolean?)
  (lf>= y x [flag #t]))

(define/contract (lfmax x y [flag #t])
  (->* (logfloat? logfloat?) (boolean?) logfloat?)
  (if (lf>= x y flag) x y))

(define/contract (lfmax1 x y)
  (-> logfloat? logfloat? logfloat?)
  (lfmax x y #f))

(define/contract (lfmin x y [flag #t])
  (->* (logfloat? logfloat?) (boolean?) logfloat?)
  (if (lf<= x y flag) x y))

(define/contract (lfmin1 x y)
  (-> logfloat? logfloat? logfloat?)
  (lfmin x y #f))

(define/contract (lfabs x)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ ex1 ex2) x)
  (let*-values ([(x1 x2) (ddabs x1 x2)])
    (logfloat x1 x2 #true ex1 ex2)))

;; QUESTION: is inf inf _ inf 0 an overflow
(define/contract (lfoverflow? x)
  (-> logfloat? boolean?)
  (lf> (lfabs x) (max.lf)))

;; QUESTION: is 0 0 _ -inf 0 an underflow
(define/contract (lfunderflow? x)
  (-> logfloat? boolean?)
  (lf< (lfabs x) (min.lf)))

(define/contract (lfover/underflowed? x)
  (-> logfloat? boolean?)
  (or (lfoverflow? x) (lfunderflow? x)))

(define/contract (lfsamesign? x y [flag #t])
  (->* (logfloat? logfloat?) (boolean?) boolean?)
  (match-define (logfloat x1 x2 sx _ _) x)
  (match-define (logfloat y1 y2 sy _ _) y)
  (if flag
      (not (xor sx sy))
      (ddsamesign? x1 x2 y1 y2)))

(define/contract (lfnan? x)
  (-> logfloat? boolean?)
  (match-define (logfloat x1 x2 _ ex1 ex2) x)
  (when (and (not (ddnan? x1 x2)) (ddnan? ex1 ex2))
    (eprintf "[ERROR::lfnan?] ~a\n" x))
  (and (ddnan? x1 x2) (ddnan? ex1 ex2)))

(define/contract (lfrepresentable? x)
  (-> logfloat? boolean?)
  (and (not (lfunderflow? x)) (not (lfoverflow? x)) (not (lfnan? x))))

(define/contract (lfneg x)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s e1 e2) x)
  (let*-values ([(x1 x2) (ddneg x1 x2)])
    (logfloat x1 x2 (not s) e1 e2)))

(define/contract (lf+ A B)
  (-> logfloat? logfloat? logfloat?)
  (define P (if (lf>= (lfabs A) (lfabs B)) A B))
  (define Q (if (lf< (lfabs A) (lfabs B)) A B))
  (match-define (logfloat x1 x2 xs ex1 ex2) P)
  (match-define (logfloat y1 y2 ys ey1 ey2) Q)
  (let*-values ([(a1 a2) (dd- ey1 ey2 ex1 ex2)]
                [(b1 b2) (ddexp2 a1 a2)]
                [(c1 c2) (if (not (xor xs ys))
                             (dd+ 1.0 0.0 b1 b2)
                             (dd- 1.0 0.0 b1 b2))]
                [(d1 d2) (ddabs c1 c2)]
                [(e1 e2) (ddlog2 d1 d2)]
                [(f1 f2) (dd+ ex1 ex2 e1 e2)]
                [(z1 z2) (dd+ x1 x2 y1 y2)])
    (logfloat z1 z2 xs f1 f2)))
;sldkjflskdjflskdjflskdjflskdjflskdjflskdjfsllskdjflskdfjshss
(define/contract (lf- A B)
  (-> logfloat? logfloat? logfloat?)
  (lf+ A (lfneg B)))

(define/contract (lf* A B)
  (-> logfloat? logfloat? logfloat?)
  (match-define (logfloat x1 x2 xs ex1 ex2) A)
  (match-define (logfloat y1 y2 ys ey1 ey2) B)
  (let*-values ([(z1 z2) (dd* x1 x2 y1 y2)]
                [(zs) (not (xor xs ys))]
                [(ez1 ez2) (dd+ ex1 ex2 ey1 ey2)])
    (logfloat z1 z2 zs ez1 ez2)))

(define/contract (lf/ A B)
  (-> logfloat? logfloat? logfloat?)
  (match-define (logfloat x1 x2 xs ex1 ex2) A)
  (match-define (logfloat y1 y2 ys ey1 ey2) B)
  (let*-values ([(z1 z2) (dd/ x1 x2 y1 y2)]
                [(zs) (not (xor xs ys))]
                [(ez1 ez2) (dd- ex1 ex2 ey1 ey2)])
    (logfloat z1 z2 zs ez1 ez2)))

(define/contract (lflog A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ ex1 ex2) A)
  (let*-values ([(z1 z2) (ddlog x1 x2)]
                [(zs) (dd>= ex1 ex2 0.0 0.0)]
                [(a1 a2) (dd* ex1 ex2 log2-hi log2-lo)]
                [(a1 a2) (ddabs a1 a2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 zs a1 a2)))

(define/contract (lflog2 A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ ex1 ex2) A)
  (let*-values ([(z1 z2) (ddlog2 x1 x2)]
                [(zs) (dd>= ex1 ex2 0.0 0.0)]
                [(a1 a2) (ddabs ex1 ex2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 zs a1 a2)))

(define/contract (lfexp A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ _ _) A)
  (let*-values ([(z1 z2) (ddexp x1 x2)]
                [(zs) #true]
                [(a1 a2) (ddlog2 dde1 dde2)]
                [(a1 a2) (dd* x1 x2 a1 a2)])
    (logfloat z1 z2 zs a1 a2)))

(define/contract (lfexpt A B)
  (-> logfloat? logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ ex1 ex2) A)
  (match-define (logfloat y1 y2 _ _ _) B)
  (let*-values ([(z1 z2) (ddexpt x1 x2 y1 y2)]
                [(zs) (dd>= z1 z2 0.0 0.0)]
                [(int?) (ddint? y1 y2)])
    (cond
      [(dd>= x1 x2 0.0 0.0)
       (let*-values ([(a1 a2) (dd* y1 y2 ex1 ex2)])
         (logfloat z1 z2 zs a1 a2))]
      [int?
       (let*-values ([(a1 a2) (dd* y1 y2 ex1 ex2)])
         (logfloat z1 z2 zs a1 a2))]
      [else (logfloat z1 z2 zs +nan.0 0.0)])))

(define/contract (lfsqrt A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ ex1 ex2) A)
  (let*-values ([(z1 z2) (ddsqrt x1 x2)]
                [(a1 a2) (dd/ ex1 ex2 2.0 0.0)])
    (logfloat z1 z2 #true a1 a2)))

(define/contract (lfcbrt A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 xs ex1 ex2) A)
  (let*-values ([(z1 z2) (ddcbrt x1 x2)]
                [(a1 a2) (dd/ ex1 ex2 3.0 0.0)])
    (logfloat z1 z2 xs a1 a2)))

(define/contract (lfcos A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ _ _) A)
  (let*-values ([(z1 z2) (ddrcos x1 x2)]
                [(zs) (dd>= z1 z2 0.0 0.0)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 zs a1 a2)))

(define/contract (lfsin A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ _ _) A)
  (let*-values ([(z1 z2) (ddrsin x1 x2)]
                [(zs) (dd>= z1 z2 0.0 0.0)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 zs a1 a2)))

(define/contract (lftan A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ _ _) A)
  (let*-values ([(z1 z2) (ddrtan x1 x2)]
                [(zs) (dd>= z1 z2 0.0 0.0)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 zs a1 a2)))

(define/contract (lffloor A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddfloor x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfceil A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddceil x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfasin A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddasin x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfacos A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddacos x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfatan A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddatan x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfsinh A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddasin x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfcosh A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddacos x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lftanh A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddatan x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfasinh A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddasin x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfacosh A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddacos x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfatanh A)
  (-> logfloat? logfloat?)
  (match-define (logfloat x1 x2 s _ _) A)
  (let*-values ([(z1 z2) (ddatan x1 x2)]
                [(a1 a2) (ddabs z1 z2)]
                [(a1 a2) (ddlog2 a1 a2)])
    (logfloat z1 z2 s a1 a2)))

(define/contract (lfatan2 A B)
  (-> logfloat? logfloat? logfloat?)
  (match-define (logfloat x1 x2 _ _ _) A)
  (match-define (logfloat y1 y2 _ _ _) B)
  (let*-values ([(z1 z2) (ddatan2 x1 x2 y1 y2)]
                [(a1 a2) (ddabs z1 z2)]
                [(ez1 ez2) (ddlog2 a1 a2)]
                [(zs) (dd>= z1 z2 0.0 0.0)])
    (logfloat z1 z2 zs ez1 ez2)))

(define (lfop symbol)
  (match symbol
    ['lf+ lf+]
    ['lf- lf-]
    ['lf* lf*]
    ['lf/ lf/]
    ['lflog lflog]
    ['lfexp lfexp]
    ['lfexpt lfexpt]
    ['lfsin lfsin]
    ['lfcos lfcos]
    ['lftan lftan]
    ['lfsqrt lfsqrt]
    ['lfcbrt lfcbrt]
    ['lfneg lfneg]
    ['lf> lf>]
    ['lf>= lf>=]
    ['lf< lf<]
    ['lf<= lf<=]
    ['lfabs lfabs]
    ['lfceil lfceil]
    ['lffloor lffloor]
    ['lfasin lfasin]
    ['lfacos lfacos]
    ['lfatan lfatan]
    ['lfsinh lfsinh]
    ['lfcosh lfcosh]
    ['lftanh lftanh]
    ['lfasinh lfasinh]
    ['lfacosh lfacosh]
    ['lfatanh lfatanh]
    ['lflog2 lflog2]
    ['lfmax lfmax]
    ['lfmin lfmin]
    ['lfatan2 lfatan2]
    [_ (error 'lfop "This should be unreachable: ~a" symbol)]))

(define (op->lfop op)
  (match op
    ['+.f64 'lf+]
    ['-.f64 'lf-]
    ['*.f64 'lf*]
    ['/.f64 'lf/]
    ['log.f64 'lflog]
    ['exp.f64 'lfexp]
    ['pow.f64 'lfexpt]
    ['sin.f64 'lfsin]
    ['cos.f64 'lfcos]
    ['tan.f64 'lftan]
    ['sqrt.f64 'lfsqrt]
    ['cbrt.f64 'lfcbrt]
    ['neg.f64 'lfneg]
    ['+.f32 'lf+]
    ['-.f32 'lf-]
    ['*.f32 'lf*]
    ['/.f32 'lf/]
    ['log.f32 'lflog]
    ['exp.f32 'lfexp]
    ['pow.f32 'lfexpt]
    ['sin.f32 'lfsin]
    ['cos.f32 'lfcos]
    ['tan.f32 'lftan]
    ['sqrt.f32 'lfsqrt]
    ['cbrt.f32 'lfcbrt]
    ['neg.f32 'lfneg]
    ['>.f64 'lf>]
    ['>.f32 'lf>]
    ['>=.f64 'lf>=]
    ['>=.f32 'lf>=]
    ['<.f64 'lf<]
    ['<.f32 'lf<]
    ['<=.f64 'lf<=]
    ['<=.f32 'lf<=]
    ['if 'if]
    ['fabs.f32 'lfabs]
    ['fabs.f64 'lfabs]
    ['ceil.f32 'lfceil]
    ['ceil.f64 'lfceil]
    ['floor.f32 'lffloor]
    ['floor.f64 'lffloor]
    ['asin.f64 'lfasin]
    ['acos.f64 'lfacos]
    ['atan.f64 'lfatan]
    ['sinh.f64 'lfsinh]
    ['cosh.f64 'lfcosh]
    ['tanh.f64 'lftanh]
    ['asinh.f64 'lfasinh]
    ['acosh.f64 'lfacosh]
    ['atanh.f64 'lfatanh]
    ['asin.f32 'lfasin]
    ['acos.f32 'lfacos]
    ['atan.f32 'lfatan]
    ['sinh.f32 'lfsinh]
    ['cosh.f32 'lfcosh]
    ['tanh.f32 'lftanh]
    ['asinh.f32 'lfasinh]
    ['acosh.f32 'lfacosh]
    ['atanh.f32 'lfatanh]
    ['fmax.f64 'lfmax]
    ['fmax.f32 'lfmax]
    ['fmin.f64 'lfmin]
    ['fmax.f32 'lfmin]
    ['log2.64 'lflog2]
    ['log2.f32 'lflog2]
    ['atan2.f64 'lfatan2]
    ['atan2.f32 'lfatan2]
    [_ (error 'op->logop "~a does not have logfloat equivalent." op)]))

(define (expr->lf expr)
  (match expr
    [(struct literal (value _))
     (lf (if (flonum? value)
             value
             (exact->inexact value)))]
    [(list (or 'PI.f64 'PI.f32)) (lf ddpi1 ddpi2)]
    [(list (or 'E.f64 'E.f32)) (lf dde1 dde2)]
    [(list op args ...) (cons (op->lfop op) (map expr->lf args))]
    [sym sym]))

(define 1.lf (lf 1.0))
