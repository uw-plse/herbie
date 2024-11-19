#lang racket

;;; C/C++ on Linux with reduced libm
;;; We use textbook mathematical operators, i.e.,
;;; no special numbers functions

(require "../plugin.rkt")

(define move-cost 0.02333600000000001)
(define fl-move-cost (* move-cost 4))

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
                 #:optional [*.f64 0.20874800000000002]
                 [+.f64 0.164604]
                 [-.f64 0.15163999999999997]
                 [/.f64 0.26615199999999994]
                 [acos.f64 0.357748]
                 [acosh.f64 0.6594719999999998]
                 [asin.f64 0.389788]
                 [asinh.f64 0.8350280000000001]
                 [atan.f64 0.8375199999999997]
                 [atan2.f64 1.4928039999999998]
                 [atanh.f64 0.3623799999999999]
                 [cbrt.f64 1.5651760000000001]
                 [ceil.f64 0.4729999999999999]
                 [copysign.f64 0.20045199999999996]
                 [cos.f64 3.3228800000000005]
                 [cosh.f64 0.9538959999999999]
                 [erf.f64 0.8064360000000002]
                 [erfc.f64 0.816512]
                 [exp.f64 1.0806]
                 [exp2.f64 0.825484]
                 [fabs.f64 0.10161999999999997]
                 [fdim.f64 0.592576]
                 [floor.f64 0.46856799999999993]
                 [fmax.f64 0.3106]
                 [fmin.f64 0.289256]
                 [fmod.f64 94.277144]
                 [lgamma.f64 1.568012]
                 [log.f64 0.5057240000000001]
                 [log10.f64 0.868856]
                 [log2.f64 0.6812760000000003]
                 [logb.f64 0.22065599999999996]
                 [neg.f64 0.09659199999999998]
                 [pow.f64 1.52482]
                 [remainder.f64 16.165012]
                 [rint.f64 0.12186400000000001]
                 [round.f64 0.6585639999999999]
                 [sin.f64 3.3181279999999997]
                 [sinh.f64 1.2095399999999996]
                 [sqrt.f64 0.19187200000000001]
                 [tan.f64 3.7109039999999998]
                 [tanh.f64 0.8240159999999999]
                 [tgamma.f64 1.8825760000000002]
                 [trunc.f64 0.46364400000000006])

(register-platform! 'math (platform-union boolean-platform non-tunable tunable))

;; Do not run this file during testing
(module test racket/base
  )
