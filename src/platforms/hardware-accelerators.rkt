#lang racket

(require "../plugin.rkt"
         math/flonum)

(define (fl2dotprod a b c d)
(define-values (x1 x2) (fl2* a 0.0 b 0.0))
(define-values (y1 y2) (fl2* c 0.0 d 0.0))
(define-values (z1 z2) (fl2+ x1 x2 y1 y2))
z1)

(define (fl2add3 a b c)
(define-values (x1 x2) (fl2+ a 0.0 b 0.0))
(define-values (y1 y2) (fl2+ x1 x2 c 0.0))
y1)

(define (fl2fma a b c)
(define-values (x1 x2) (fl2* a 0.0 b 0.0))
(define-values (y1 y2) (fl2+ x1 x2 c 0.0))
y1)

(define (square a) (* a a))

(define-operator-impl (dotprod.f32 [a : binary32] [b : binary32] [c : binary32] [d : binary32])
                      binary32
                      #:spec  (+ (* a b) (* c d))
                      #:fpcore (! :precision binary32 (dotprod a b c d))
                      #:fl fl2dotprod)

(define-operator-impl (add3.f32 [a : binary32] [b : binary32] [c : binary32])
                      binary32
                      #:spec (+ (+ a b) c)
                      #:fpcore (! :precision binary32 (add3 a b c))
                      #:fl fl2add3)

;;; (define-operator-impl (square1p.f32 [a : binary32])
;;;                       binary32
;;;                       #:spec (+ (* a a) 1)
;;;                       #:fpcore (! :precision binary32 (square1p a)))
;;;                       ;fma

;;; (define-operator-impl (sumofsquares.f32 [a : binary32] [b : binary32])
;;;                       binary32
;;;                       #:spec (+ (* a a) (* b b))
;;;                       #:fpcore (! :precision binary32 (sumofsquares a b)))   


(define-operator-impl (fma.f32 [a : binary32] [b : binary32] [c : binary32])
                      binary32
                      #:spec (+ (* a b) c)
                      #:fpcore (! :precision binary32 (fma a b c))
                      #:fl fl2fma)

(define-operator-impl (square.f32 [a : binary32])
                      binary32
                      #:spec (* a a)
                      #:fpcore (! :precision binary32 (square a))
                      #:fl square)

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
                 #:literal [binary32 1]
                 [PI.f32 1]
                 [E.f32 1]
                 [INFINITY.f32 1]
                 [NAN.f32 1]
                 [==.f32 compare-cost]
                 [!=.f32 compare-cost]
                 [>.f32 compare-cost]
                 [<.f32 compare-cost]
                 [>=.f32 compare-cost]
                 [<=.f32 compare-cost]
                 [fabs.f32 64]
                 [fmax.f32 3200]
                 [fmin.f32 3200]
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
                 [fma.f32 3.960759494]
                 [square.f32 1.82278481]
)

(define hardware-accelerators-platform-2 (platform-union boolean-platform hardware-accelerators-platform))

(register-platform! 'hardware-accelerators hardware-accelerators-platform-2)

                   

