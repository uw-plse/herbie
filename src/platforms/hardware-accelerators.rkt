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
                      #:fpcore (! :precision binary32 (square1p a b)))   


(define-operator-impl (fma.f32 [a : binary32] [b : binary32] [c : binary32])
                      binary32
                      #:spec (+ (* a b) c)
                      #:fpcore (! :precision binary32 (square1p a b)))  
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

(define-platform hardware-accelerators-platform
                 #:literal [binary32 32]
                 [PI.f32 32]
                 [E.f32 32]
                 [INFINITY.f32 32]
                 [NAN.f32 32]
                 [neg.f32 64]
                 [+.f32 64]
                 [-.f32 64]
                 [*.f32 128]
                 [/.f32 320]
                 [==.f32 128]
                 [!=.f32 128]
                 [>.f32 128]
                 [<.f32 128]
                 [>=.f32 128]
                 [<=.f32 128]
            
                [sqrt.f32 0.38716720000000004]
                [dotprod.f32 0.1]
                [add3.f32 0.1]
                [square1p.f32 0.1]
                [sumofsquares.f32 0.1]
                [fma.f32 0.1])



(define hardware-accelerators-platform-2 (platform-union boolean-platform hardware-accelerators-platform))

(register-platform! 'hardware-accelerators hardware-accelerators-platform-2)

                   

