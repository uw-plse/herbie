#lang racket

(require "../plugin.rkt")
; universal boolean operations

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
                      #:fpcore (! :precision binary32 (sumofsquares a b)))   


(define-operator-impl (fma.f32 [a : binary32] [b : binary32] [c : binary32])
                      binary32
                      #:spec (+ (* a b) c)
                      #:fpcore (! :precision binary32 (fma a b c)))  
; universal boolean operations
; universal boolean opertaions
(define-platform boolean-platform
                 #:literal [bool 1]
                 #:default-cost 1
                 #:if-cost 1
                 TRUE
                 FALSE
                 not
                 and
                 or)

(define compare-cost 0.1506329114)
(define-platform hardware-accelerators-platform
                 #:literal [binary32 0]
                 [PI.f32 0]
                 [E.f32 0]
                 [INFINITY.f32 0]
                 [NAN.f32 0]
                 [==.f32 compare-cost]
                 [!=.f32 compare-cost]
                 [>.f32 compare-cost]
                 [<.f32 compare-cost]
                 [>=.f32 compare-cost]
                 [<=.f32 compare-cost]
                ;;;  [fabs.f32 64]
                ;;;  [fmax.f32 3200]
                ;;;  [fmin.f32 3200]
                 [sqrt.f32 1.93164557]
                 [neg.f32 0.7556962025]
                 [+.f32 1]
                 [-.f32 0.7556962025]
                 [*.f32 3.191139241]
                 [/.f32 4.729113924]

                 [dotprod.f32 3.582278481]
                 [add3.f32 1.33164557]
                ;;;  [square1p.f32 0.1]
                ;;;  [sumofsquares.f32 0.1]
                 [fma.f32 3.960759494])

(define hardware-accelerators-platform-2 (platform-union boolean-platform hardware-accelerators-platform))

(register-platform! 'hardware-accelerators hardware-accelerators-platform-2)

                   

