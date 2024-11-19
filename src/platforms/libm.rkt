#lang racket

;;; The default platform:
;;; C/C++ on Linux with a full libm

(require "../plugin.rkt")

(define fl-move-cost 0.1522809999999996)
(define move-cost fl-move-cost)

; universal boolean operations
(define-platform boolean-platform
                 #:literal [bool move-cost]
                 #:default-cost move-cost
                 #:if-cost move-cost
                 TRUE.bool
                 FALSE.bool
                 not.bool
                 and.bool
                 or.bool)

; non-tunable operations
(define-platform non-tunable
                 #:literal [binary64 fl-move-cost]
                 #:default-cost fl-move-cost
                 PI.f64
                 E.f64
                 INFINITY.f64
                 NAN.f64
                 ==.f64
                 !=.f64
                 >.f64
                 <.f64
                 >=.f64
                 <=.f64)

; tunable operations
(define-platform tunable
                 #:literal [binary64 fl-move-cost]
                 #:optional [*.f64 0.284122]
                 [+.f64 0.21685800000000022]
                 [-.f64 0.2448160000000003]
                 [/.f64 0.3915630999999998]
                 [acos.f64 0.8382765000000001]
                 [acosh.f64 1.0949965999999995]
                 [asin.f64 0.8879643999999998]
                 [asinh.f64 1.3600788]
                 [atan.f64 1.4839419]
                 [atan2.f64 5.758423799999999]
                 [atanh.f64 0.7060143]
                 [cbrt.f64 2.2140749]
                 [ceil.f64 0.4065622999999997]
                 [copysign.f64 0.23500700000000005]
                 [cos.f64 4.814481299999999]
                 [cosh.f64 1.3792117999999995]
                 [erf.f64 1.1157903000000002]
                 [erfc.f64 0.8191463000000002]
                 [exp.f64 1.7585839]
                 [exp2.f64 1.2538337]
                 [expm1.f64 1.0743543999999998]
                 [fabs.f64 0.15341399999999988]
                 [fdim.f64 0.9574675000000002]
                 [floor.f64 0.40936829999999985]
                 [fma.f64 0.4402650999999995]
                 [fmax.f64 0.24361820000000006]
                 [fmin.f64 0.2473790000000003]
                 [fmod.f64 1.9713020000000003]
                 [hypot.f64 1.3146293999999996]
                 [lgamma.f64 2.270068099999999]
                 [log.f64 0.6301383000000002]
                 [log10.f64 1.1023494999999996]
                 [log1p.f64 1.4341258000000001]
                 [log2.f64 0.7708615]
                 [logb.f64 0.484051]
                 [neg.f64 0.1515359999999996]
                 [pow.f64 2.5363692]
                 [remainder.f64 1.5360727999999997]
                 [rint.f64 0.36840199999999984]
                 [round.f64 0.9615382000000002]
                 [sin.f64 4.732287400000001]
                 [sinh.f64 1.7850157999999996]
                 [sqrt.f64 0.38716720000000004]
                 [tan.f64 6.046834999999998]
                 [tanh.f64 1.2760664999999998]
                 [tgamma.f64 2.8195002000000007]
                 [trunc.f64 0.3390401999999998])

(register-platform! 'c (platform-union boolean-platform non-tunable tunable))

;; Do not run this file during testing
(module test racket/base
  )
