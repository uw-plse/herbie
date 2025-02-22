#lang racket

(require rival math/bigfloat) 
(define expr '(- (sin.f64 x) (- x (/ (pow x 3) 6))))
(define machine (rival-compile (list expr) '(x) (list flonum-discretization)))
(rival-apply machine (vector (bf 0.5)))
(rival-apply machine (vector (bf 1e-100)))
