#lang racket

(require "load-plugin.rkt"
         "syntax.rkt"
         "platform.rkt"
         "types.rkt"
         (only-in "platform.rkt" *active-platform* activate-platform!)
         (submod "syntax.rkt" internals)
         (submod "platform.rkt" internals))

(module+ test
  (require rackunit
           math/bigfloat)

  (load-herbie-builtins)

  ; log1pmd(x) = log1p(x) - log1p(-x)

  (define-operator-impl (log1pmd.f64 [x : binary64])
                        binary64
                        #:spec (- (log (+ 1 x)) (log (+ 1 (neg x))))
                        #:fpcore (! :precision binary64 (log1pmd x)))

  (define log1pmd-proc (impl-info 'log1pmd.f64 'fl))
  (define log1pmd-vals '((0.0 . 0.0) (0.5 . 1.0986122886681098) (-0.5 . -1.0986122886681098)))
  (for ([(pt out) (in-dict log1pmd-vals)])
    (check-equal? (log1pmd-proc pt) out (format "log1pmd(~a) = ~a" pt out)))

  ; fast sine

  (define-operator-impl (fast-sin.f64 [x : binary64])
                        binary64
                        #:spec (sin x)
                        #:fpcore (! :precision binary64 :math-library fast (sin x))
                        #:fl (lambda (x)
                               (parameterize ([bf-precision 12])
                                 (bigfloat->flonum (bfsin (bf x))))))

  (define sin-proc (impl-info 'fast-sin.f64 'fl))
  (define sin-vals '((0.0 . 0.0) (1.0 . 0.841552734375) (-1.0 . -0.841552734375)))
  (for ([(pt out) (in-dict sin-vals)])
    (check-equal? (sin-proc pt) out (format "sin(~a) = ~a" pt out)))

  (define-platform test-platform [log1pmd.f64 6400] [fast-sin.f64 6400])
  (register-platform! 'test (platform-union (*active-platform*) test-platform))
  (activate-platform! 'test)

  ; get-fpcore-impl

  (define f64 (get-representation 'binary64))
  (define (get-impl op props itypes)
    (get-fpcore-impl op props itypes))

  (check-equal? (get-impl '+ '((:precision . binary64)) (list f64 f64)) '+.f64)
  (check-equal? (get-impl '+ '((:precision . binary64)) (list f64 f64)) '+.f64)
  (check-equal? (get-impl '+ '((:precision . binary64) (:description . "test")) (list f64 f64))
                '+.f64)

  (check-equal? (get-impl 'log1pmd '((:precision . binary64)) (list f64)) 'log1pmd.f64)
  (check-equal? (get-impl 'sin '((:precision . binary64)) (list f64)) 'sin.f64)
  (check-equal? (get-impl 'sin '((:precision . binary64) (:math-library . fast)) (list f64))
                'fast-sin.f64)

  (void))
