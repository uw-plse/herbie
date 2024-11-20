#lang racket

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
(define operators
  (with-terminal-cost ([binary64 fl-move-cost])
    (platform-product
      [([real binary64] [bool bool])
       (cost-map #:default-cost fl-move-cost)]
      (operator-set
        [(real) (PI E INFINITY NAN)]
         [(real real) (neg fabs sqrt square1p)]
        [(real real bool) (== != > < >= <=)]
        [(real real real) (+ - * / fmax fmin)]
        [(real real real real) (add3 fma sumofsquares)]
        [(real real real real real) (dotprod)]))))

(define cost-model
  (cost-map
    [* 0.284122]
    [+ 0.21685800000000022]
    [- 0.2448160000000003]
    [/ 0.3915630999999998]

    [fabs 0.15341399999999988]
 
    [fmax 0.24361820000000006]
    [fmin 0.2473790000000003]
  
    [neg 0.1515359999999996]
 
    [sqrt 0.38716720000000004]
    [dotprod 0.1]
    [add3 0.1]
    [square1p 0.1]
    [sumofsquares 0.1]
    [fma 0.1]
 
))

(define-operator-impl (dotprod.f32 [a : binary32] [b : binary32] [c : binary32] [d : binary32])
                      binary32
                      #:spec  (+ (* a b) (* c d))
                      #:fpcore (! :precision binary32 (dotprod a b c d)))

(define-operator-impl (add3.f32 [a : binary32] [b : binary32] [c : binary32])
                      binary32
                      #:spec (+ (+ a b) c)
                      #:fpcore (! :precision binary32 (add3 a b c)))


(define-operator-impl (square1p.f32 [a : binary32])
                      binary32
                      #:spec (+ (* a a) 1)
                      #:fpcore (! :precision binary32 (square1p a)))


(define-operator-impl (sumofsquares.f32 [a : binary32] [b : binary32])
                      binary32
                      #:spec (+ (* a a) (* b b))
                      #:fpcore (! :precision binary32 (square1p a b)))   


(define-operator-impl (fma.f32 [a : binary32] [b : binary32] [c : binary32])
                      binary32
                      #:spec (+ (* a b) c)
                      #:fpcore (! :precision binary32 (square1p a b)))  

(register-platform! 'hardware
                    (platform-union boolean-platform
                                    operators))

                   

