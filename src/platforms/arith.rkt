#lang racket

;;; Arithmetic in platform

(define move-cost 0.02333600000000001)
(define fl-move-cost (* move-cost 4))

; universal boolean operations
(define boolean-platform
  (with-terminal-cost ([bool move-cost])
    (platform
      #:default-cost move-cost
      #:if-cost move-cost
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; non-tunable operations
(define non-tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost fl-move-cost)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
        [(real real bool) (== != > < >= <=)]))))

(define cost-model
  (cost-map
    [* 0.284122]
    [+ 0.21685800000000022]
    [- 0.2448160000000003]
    [/ 0.3915630999999998]
    ; [acos 0.8382765000000001]
    ; [acosh 1.0949965999999995]
    ; [asin 0.8879643999999998]
    ; [asinh 1.3600788]
    ; [atan 1.4839419]
    ; [atan2 5.758423799999999]
    ; [atanh 0.7060143]
    ; [cbrt 2.2140749]
    ; [ceil 0.4065622999999997]
    ; [copysign 0.23500700000000005]
    ; [cos 4.814481299999999]
    ; [cosh 1.3792117999999995]
    ; [erf 1.1157903000000002]
    ; [erfc 0.8191463000000002]
    ; [exp 1.7585839]
    ; [exp2 1.2538337]
    ; [expm1 1.0743543999999998]
    [fabs 0.15341399999999988]
    ; [fdim 0.9574675000000002]
    ; [floor 0.40936829999999985]
    ; [fma 0.4402650999999995]
    [fmax 0.24361820000000006]
    [fmin 0.2473790000000003]
    ; [fmod 1.9713020000000003]
    ; [hypot 1.3146293999999996]
    ; [lgamma 2.270068099999999]
    ; [log 0.6301383000000002]
    ; [log10 1.1023494999999996]
    ; [log1p 1.4341258000000001]
    ; [log2 0.7708615]
    ; [logb 0.484051]
    [neg 0.1515359999999996]
  ;   [pow 2.5363692]
  ;   [remainder 1.5360727999999997]
  ;   [rint 0.36840199999999984]
  ;   [round 0.9615382000000002]
  ;   [sin 4.732287400000001]
  ;   [sinh 1.7850157999999996]
    [sqrt 0.38716720000000004]
  ;   [tan 6.046834999999998]
  ;   [tanh 1.2760664999999998]
  ;   [tgamma 2.8195002000000007]
  ;   [trunc 0.3390401999999998]
))

; tunable operations
(define tunable
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      #:optional
      [([real binary64]) cost-model]
      (operator-set
        [(real real) (neg fabs sqrt)]
        [(real real real) (+ - * / fmax fmin)]))))

(register-platform! 'arith
                    (platform-union boolean-platform
                                    non-tunable
                                    tunable))