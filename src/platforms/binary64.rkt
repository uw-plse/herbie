#lang racket

;; Builtin double-precision plugin (:precision binary64)

(require math/flonum
         math/bigfloat)

(require "runtime/utils.rkt"
         "runtime/libm.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (binary64 real flonum?)
                       bigfloat->flonum
                       bf
                       (shift 63 ordinal->flonum)
                       (unshift 63 flonum->ordinal)
                       64
                       (conjoin number? nan?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants binary64
                  [PI PI.f64 pi]
                  [E E.f64 (exp 1.0)]
                  [INFINITY INFINITY.f64 +inf.0]
                  [NAN NAN.f64 +nan.0])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-libm-impl/binary64 stx)
  (syntax-case stx (real)
    [(_ op (itype ...) otype attrib ...)
     (with-syntax ([impl (string->symbol (format "~a.f64" (syntax->datum #'op)))])
       #'(define-libm-impl op (op impl itype ...) otype attrib ...))]))

(define-syntax-rule (define-libm-impls/binary64* (itype ... otype) name ...)
  (begin
    (define-libm-impl/binary64 name (itype ...) otype) ...))

(define-syntax-rule (define-libm-impls/binary64 [(itype ... otype) (name ...)] ...)
  (begin
    (define-libm-impls/binary64* (itype ... otype) name ...) ...))

(define-operator-impl (neg.f64 [x : binary64])
                      binary64
                      #:spec (neg x)
                      #:fpcore (! :precision binary64 (- x))
                      #:fl -
                      #:identities
                      ([distribute-lft-neg-in (neg.f64 (*.f64 a b)) (*.f64 (neg.f64 a) b)]
                       [distribute-rgt-neg-in (neg.f64 (*.f64 a b)) (*.f64 a (neg.f64 b))]
                       [distribute-neg-in (neg.f64 (+.f64 a b)) (+.f64 (neg.f64 a) (neg.f64 b))]
                       [distribute-neg-frac (neg.f64 (/.f64 a b)) (/.f64 (neg.f64 a) b)]
                       [distribute-neg-frac2 (neg.f64 (/.f64 a b)) (/.f64 a (neg.f64 b))]
                       [remove-double-neg (neg.f64 (neg.f64 a)) a]
                       [neg-sub0 (neg.f64 b) (-.f64 0 b)]
                       [neg-mul-1 (neg.f64 a) (*.f64 -1 a)]))
(define-operator-impl (+.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (+ x y)
                      #:fpcore (! :precision binary64 (+ x y))
                      #:fl +
                      #:commutes
                      #:identities
                      ([distribute-neg-out (+.f64 (neg.f64 a) (neg.f64 b)) (neg.f64 (+.f64 a b))]
                       [+-lft-identity (+.f64 0 a) a]
                       [+-rgt-identity (+.f64 a 0) a]
                       [unsub-neg (+.f64 a (neg.f64 b)) (-.f64 a b)]))
(define-operator-impl (-.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (- x y)
                      #:fpcore (! :precision binary64 (- x y))
                      #:fl -
                      #:identities
                      ([cancel-sign-sub (-.f64 a (*.f64 (neg.f64 b) c)) (+.f64 a (*.f64 b c))]
                       [cancel-sign-sub-inv (-.f64 a (*.f64 b c)) (+.f64 a (*.f64 (neg.f64 b) c))]
                       [sub-cancel (-.f64 a a) 0]
                       [sub-fold (-.f64 a 0) a]
                       [sub0-neg (-.f64 0 a) (neg.f64 a)]
                       [sub-neg (-.f64 a b) (+.f64 a (neg.f64 b))]))
(define-operator-impl
 (*.f64 [x : binary64] [y : binary64])
 binary64
 #:spec (* x y)
 #:fpcore (! :precision binary64 (* x y))
 #:fl *
 #:commutes
 #:identities ([distribute-lft-neg-out (*.f64 (neg.f64 a) b) (neg.f64 (*.f64 a b))]
               [distribute-rgt-neg-out (*.f64 a (neg.f64 b)) (neg.f64 (*.f64 a b))]
               [mul0-lft (*.f64 0 a) 0]
               [mul0-rgt (*.f64 a 0) 0]
               [*-lft-identity (*.f64 1 a) a]
               [*-rgt-identity (*.f64 a 1) a]
               [mul-1-neg (*.f64 -1 a) (neg.f64 a)]
               [*-un-lft-identity a (*.f64 1 a)]
               [sqr-neg (*.f64 (neg.f64 a) (neg.f64 a)) (*.f64 a a)]
               [sqr-abs (*.f64 (fabs.f64 a) (fabs.f64 a)) (*.f64 a a)]
               [mul-fabs (*.f64 (fabs.f64 a) (fabs.f64 b)) (fabs.f64 (*.f64 a b))]
               [sqr-sin-b (*.f64 (sin.f64 x) (sin.f64 x)) (-.f64 1 (*.f64 (cos.f64 x) (cos.f64 x)))]
               [sqr-cos-b (*.f64 (cos.f64 x) (cos.f64 x)) (-.f64 1 (*.f64 (sin.f64 x) (sin.f64 x)))]))
(define-operator-impl (/.f64 [x : binary64] [y : binary64])
                      binary64
                      #:spec (/ x y)
                      #:fpcore (! :precision binary64 (/ x y))
                      #:fl /
                      #:identities
                      ([distribute-frac-neg (/.f64 (neg.f64 a) b) (neg.f64 (/.f64 a b))]
                       [distribute-frac-neg2 (/.f64 a (neg.f64 b)) (neg.f64 (/.f64 a b))]
                       [div0 (/.f64 0 a) 0]
                       [*-inverses (/.f64 a a) 1]
                       [/-rgt-identity (/.f64 a 1) a]
                       [div-fabs (/.f64 (fabs.f64 a) (fabs.f64 b)) (fabs.f64 (/.f64 a b))]
                       [inv-pow (/.f64 1 a) (pow.f64 a -1)]))

(define-libm-impl/binary64 fabs
                           (binary64)
                           binary64
                           #:identities
                           ([fabs-fabs (fabs.f64 (fabs.f64 a)) (fabs.f64 a)]
                            [fabs-sub (fabs.f64 (-.f64 a b)) (fabs.f64 (-.f64 b a))]
                            [fabs-neg (fabs.f64 (neg.f64 a)) (fabs.f64 a)]
                            [fabs-sqr (fabs.f64 (*.f64 a a)) (*.f64 a a)]
                            [fabs-mul (fabs.f64 (*.f64 a b)) (*.f64 (fabs.f64 a) (fabs.f64 b))]
                            [fabs-div (fabs.f64 (/.f64 a b)) (/.f64 (fabs.f64 a) (fabs.f64 b))]
                            [neg-fabs (fabs.f64 x) (fabs.f64 (neg.f64 x))]))

(define-libm-impl/binary64 exp
                           (binary64)
                           binary64
                           #:identities ([exp-0 (exp.f64 0) 1] [exp-1-e (exp.f64 1) (E)]
                                                               [1-exp 1 (exp.f64 0)]
                                                               [e-exp-1 (E) (exp.f64 1)]))

(define-libm-impl/binary64 pow
                           (binary64 binary64)
                           binary64
                           #:identities ([unpow1 (pow.f64 a 1) a] [unpow0 (pow.f64 a 0) 1]
                                                                  [pow-base-1 (pow.f64 1 a) 1]
                                                                  [pow1 a (pow.f64 a 1)]
                                                                  [pow-base-0 (pow.f64 0 a) 0]))

(define-libm-impl/binary64
 sin
 (binary64)
 binary64
 #:identities ([sin-0 (sin.f64 0) 0] [sin-neg (sin.f64 (neg.f64 x)) (neg.f64 (sin.f64 x))]))

(define-libm-impl/binary64 cos
                           (binary64)
                           binary64
                           #:identities
                           ([cos-0 (cos.f64 0) 1] [cos-neg (cos.f64 (neg.f64 x)) (cos.f64 x)]))

(define-libm-impl/binary64
 tan
 (binary64)
 binary64
 #:identities ([tan-0 (tan.f64 0) 0] [tan-neg (tan.f64 (neg.f64 x)) (neg.f64 (tan.f64 x))]))

(define-libm-impl/binary64 sinh
                           (binary64)
                           binary64
                           #:identities ([sinh-neg (sinh.f64 (neg.f64 x)) (neg.f64 (sinh.f64 x))]
                                         [sinh-0 (sinh.f64 0) 0]))

(define-libm-impl/binary64 cosh
                           (binary64)
                           binary64
                           #:identities
                           ([cosh-neg (cosh.f64 (neg.f64 x)) (cosh.f64 x)] [cosh-0 (cosh.f64 0) 1]))

(define-libm-impls/binary64 [(binary64 binary64)
                             (acos acosh
                                   asin
                                   asinh
                                   atan
                                   atanh
                                   cbrt
                                   ceil
                                   cosh
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
                                   sinh
                                   sqrt
                                   tanh
                                   tgamma
                                   trunc)]
                            [(binary64 binary64 binary64)
                             (atan2 copysign fdim fmax fmin fmod pow remainder)])

(define-libm c_erfc (erfc double double))
(define-libm c_expm1 (expm1 double double))
(define-libm c_log1p (log1p double double))
(define-libm c_hypot (hypot double double double))
(define-libm c_fma (fma double double double double))

(when c_erfc
  (define-operator-impl (erfc.f64 [x : binary64])
                        binary64
                        #:spec (- 1 (erf x))
                        #:fpcore (! :precision binary64 (erfc x))
                        #:fl c_erfc))

(when c_expm1
  (define-operator-impl (expm1.f64 [x : binary64])
                        binary64
                        #:spec (- (exp x) 1)
                        #:fpcore (! :precision binary64 (expm1 x))
                        #:fl c_expm1))

(when c_log1p
  (define-operator-impl (log1p.f64 [x : binary64])
                        binary64
                        #:spec (log (+ 1 x))
                        #:fpcore (! :precision binary64 (log1p x))
                        #:fl c_log1p))

(when c_hypot
  (define-operator-impl (hypot.f64 [x : binary64] [y : binary64])
                        binary64
                        #:spec (sqrt (+ (* x x) (* y y)))
                        #:fpcore (! :precision binary64 (hypot x y))
                        #:fl c_hypot))

(when c_fma
  (define-operator-impl (fma.f64 [x : binary64] [y : binary64] [z : binary64])
                        binary64
                        #:spec (+ (* x y) z)
                        #:fpcore (! :precision binary64 (fma x y z))
                        #:fl c_fma))

(define-comparator-impls binary64
                         [== ==.f64 =]
                         [!= !=.f64 (negate =)]
                         [< <.f64 <]
                         [> >.f64 >]
                         [<= <=.f64 <=]
                         [>= >=.f64 >=])
