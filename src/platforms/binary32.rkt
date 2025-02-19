#lang racket

;; A 32-bit float is emulated in Racket as a 64-bit float,
(require math/bigfloat
         math/flonum)
(require "runtime/float32.rkt"
         "runtime/utils.rkt"
         "runtime/libm.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-representation (binary32 real flonum?)
                       bigfloat->float32
                       bf
                       (shift 31 ordinal->float32)
                       (unshift 31 float32->ordinal)
                       32
                       (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary32
                  [PI PI.f32 (flsingle pi)]
                  [E E.f32 (flsingle (exp 1.0))]
                  [INFINITY INFINITY.f32 +inf.0]
                  [NAN NAN.f32 +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-impl/binary32 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f32" (syntax->datum #'op)))]
                   [cname (string->symbol (format "~af" (syntax->datum #'op)))])
       #'(define-libm-impl cname (op impl itype ...) otype attrib ...))]))

(define-syntax-rule (define-libm-impls/binary32* (itype ... otype) name ...)
  (begin
    (define-libm-impl/binary32 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary32 [(itype ... otype) (name ...)] ...)
  (begin
    (define-libm-impls/binary32* (itype ... otype) name ...) ...))

(define-operator-impl (neg.f32 [x : binary32])
                      binary32
                      #:spec (neg x)
                      #:fpcore (! :precision binary32 (- x))
                      #:fl fl32-
                      #:identities
                      ([distribute-lft-neg-in (neg.f32 (*.f32 a b)) (*.f32 (neg.f32 a) b)]
                       [distribute-rgt-neg-in (neg.f32 (*.f32 a b)) (*.f32 a (neg.f32 b))]
                       [distribute-neg-in (neg.f32 (+.f32 a b)) (+.f32 (neg.f32 a) (neg.f32 b))]
                       [distribute-neg-frac (neg.f32 (/.f32 a b)) (/.f32 (neg.f32 a) b)]
                       [distribute-neg-frac2 (neg.f32 (/.f32 a b)) (/.f32 a (neg.f32 b))]
                       [remove-double-neg (neg.f32 (neg.f32 a)) a]
                       [neg-sub0 (neg.f32 b) (-.f32 0 b)]
                       [neg-mul-1 (neg.f32 a) (*.f32 -1 a)]))

(define-operator-impl (+.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (+ x y)
                      #:fpcore (! :precision binary32 (+ x y))
                      #:fl fl32+
                      #:commutes
                      #:identities
                      ([distribute-neg-out (+.f32 (neg.f32 a) (neg.f32 b)) (neg.f32 (+.f32 a b))]
                       [+-lft-identity (+.f32 0 a) a]
                       [+-rgt-identity (+.f32 a 0) a]
                       [unsub-neg (+.f32 a (neg.f32 b)) (-.f32 a b)]))

(define-operator-impl (-.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (- x y)
                      #:fpcore (! :precision binary32 (- x y))
                      #:fl fl32-
                      #:identities
                      ([cancel-sign-sub (-.f32 a (*.f32 (neg.f32 b) c)) (+.f32 a (*.f32 b c))]
                       [cancel-sign-sub-inv (-.f32 a (*.f32 b c)) (+.f32 a (*.f32 (neg.f32 b) c))]
                       [sub-cancel (-.f32 a a) 0]
                       [sub-fold (-.f32 a 0) a]
                       [sub0-neg (-.f32 0 a) (neg.f32 a)]
                       [sub-neg (-.f32 a b) (+.f32 a (neg.f32 b))]))

(define-operator-impl (*.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (* x y)
                      #:fpcore (! :precision binary32 (* x y))
                      #:fl fl32*
                      #:commutes
                      #:identities
                      ([distribute-lft-neg-out (*.f32 (neg.f32 x) y) (neg.f32 (*.f32 x y))]
                       [distribute-rgt-neg-out (*.f32 x (neg.f32 y)) (neg.f32 (*.f32 x y))]
                       [*-lft-identity (*.f32 1 a) a]
                       [*-rgt-identity (*.f32 a 1) a]
                       [mul-1-neg (*.f32 -1 a) (neg.f32 a)]
                       [*-un-lft-identity a (*.f32 1 a)]
                       [sqr-neg (*.f32 (neg.f32 x) (neg.f32 x)) (*.f32 x x)]
                       [sqr-abs (*.f32 (fabs.f32 x) (fabs.f32 x)) (*.f32 x x)]
                       [mul-fabs (*.f32 (fabs.f32 a) (fabs.f32 b)) (fabs.f32 (*.f32 a b))]))

(define-operator-impl (/.f32 [x : binary32] [y : binary32])
                      binary32
                      #:spec (/ x y)
                      #:fpcore (! :precision binary32 (/ x y))
                      #:fl fl32/
                      #:identities ([distribute-frac-neg (/.f32 (neg.f32 x) y) (neg.f32 (/.f32 x y))]
                                    [distribute-frac-neg2 (/.f32 x (neg.f32 y)) (neg.f32 (/.f32 x y))]
                                    [/-rgt-identity (/.f32 a 1) a]))

(define-libm-impl/binary32 fabs
                           (binary32)
                           binary32
                           #:identities
                           ([fabs-fabs (fabs.f32 (fabs.f32 a)) (fabs.f32 a)]
                            [fabs-sub (fabs.f32 (-.f32 a b)) (fabs.f32 (-.f32 b a))]
                            [fabs-neg (fabs.f32 (neg.f32 a)) (fabs.f32 a)]
                            [fabs-sqr (fabs.f32 (*.f32 a a)) (*.f32 a a)]
                            [fabs-mul (fabs.f32 (*.f32 a b)) (*.f32 (fabs.f32 a) (fabs.f32 b))]
                            [fabs-div (fabs.f32 (/.f32 a b)) (/.f32 (fabs.f32 a) (fabs.f32 b))]
                            [neg-fabs (fabs.f32 x) (fabs.f32 (neg.f32 x))]))

(define-libm-impl/binary32 exp
                           (binary32)
                           binary32
                           #:identities ([exp-0 (exp.f32 0) 1] [exp-1-e (exp.f32 1) (E)]
                                                               [1-exp 1 (exp.f32 0)]
                                                               [e-exp-1 (E) (exp.f32 1)]))

(define-libm-impl/binary32 pow
                           (binary32 binary32)
                           binary32
                           #:identities ([unpow1 (pow.f32 a 1) a] [unpow0 (pow.f32 a 0) 1]
                                                                  [pow-base-1 (pow.f32 1 a) 1]
                                                                  [pow1 a (pow.f32 a 1)]
                                                                  [pow-base-0 (pow.f32 0 a) 0]))

(define-libm-impl/binary32
 sin
 (binary32)
 binary32
 #:identities ([sin-0 (sin.f32 0) 0] [sin-neg (sin.f32 (neg.f32 x)) (neg.f32 (sin.f32 x))]))

(define-libm-impl/binary32 cos
                           (binary32)
                           binary32
                           #:identities
                           ([cos-0 (cos.f32 0) 1] [cos-neg (cos.f32 (neg.f32 x)) (cos.f32 x)]))

(define-libm-impl/binary32
 tan
 (binary32)
 binary32
 #:identities ([tan-0 (tan.f32 0) 0] [tan-neg (tan.f32 (neg.f32 x)) (neg.f32 (tan.f32 x))]))

(define-libm-impl/binary32 sinh
                           (binary32)
                           binary32
                           #:identities ([sinh-neg (sinh.f32 (neg.f32 x)) (neg.f32 (sinh.f32 x))]
                                         [sinh-0 (sinh.f32 0) 0]))

(define-libm-impl/binary32 cosh
                           (binary32)
                           binary32
                           #:identities
                           ([cosh-neg (cosh.f32 (neg.f32 x)) (cosh.f32 x)] [cosh-0 (cosh.f32 0) 1]))

(define-comparator-impls binary32
                         [== ==.f32 =]
                         [!= !=.f32 (negate =)]
                         [< <.f32 <]
                         [> >.f32 >]
                         [<= <=.f32 <=]
                         [>= >=.f32 >=])

(define-libm-impls/binary32 [(binary32 binary32)
                             (acos acosh
                                   asin
                                   asinh
                                   atan
                                   atanh
                                   cbrt
                                   ceil
                                   erf
                                   exp
                                   exp2
                                   floor
                                   lgamma
                                   log
                                   log10
                                   log2
                                   logb
                                   rint
                                   round
                                   sqrt
                                   tanh
                                   tgamma
                                   trunc)]
                            [(binary32 binary32 binary32)
                             (atan2 copysign fdim fmax fmin fmod pow remainder)])

(define-libm c_erfcf (erfcf float float))
(define-libm c_expm1f (expm1f float float))
(define-libm c_log1pf (log1pf float float))
(define-libm c_hypotf (hypotf float float float))
(define-libm c_fmaf (fmaf float float float float))

(when c_erfcf
  (define-operator-impl (erfc.f32 [x : binary32])
                        binary32
                        #:spec (- 1 (erf x))
                        #:fpcore (! :precision binary32 (erfc x))
                        #:fl c_erfcf))

(when c_expm1f
  (define-operator-impl (expm1.f32 [x : binary32])
                        binary32
                        #:spec (- (exp x) 1)
                        #:fpcore (! :precision binary32 (expm1 x))
                        #:fl c_expm1f))

(when c_log1pf
  (define-operator-impl (log1p.f32 [x : binary32])
                        binary32
                        #:spec (log (+ 1 x))
                        #:fpcore (! :precision binary32 (log1p x))
                        #:fl c_log1pf))

(when c_hypotf
  (define-operator-impl (hypot.f32 [x : binary32] [y : binary32])
                        binary32
                        #:spec (sqrt (+ (* x x) (* y y)))
                        #:fpcore (! :precision binary32 (hypot x y))
                        #:fl c_hypotf))

(when c_fmaf
  (define-operator-impl (fma.f32 [x : binary32] [y : binary32] [z : binary32])
                        binary32
                        #:spec (+ (* x y) z)
                        #:fpcore (! :precision binary32 (fma x y z))
                        #:fl c_fmaf))

(define-operator-impl (binary64->binary32 [x : binary64])
                      binary32
                      #:spec x
                      #:fpcore (! :precision binary32 (cast x))
                      #:fl flsingle)

(define-operator-impl (binary32->binary64 [x : binary32])
                      binary64
                      #:spec x
                      #:fpcore (! :precision binary64 (cast x))
                      #:fl identity)
