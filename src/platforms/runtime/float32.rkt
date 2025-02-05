#lang racket

;; Single-precision floating-point library
;; Racket CS only has double-precision numbers but can do
;; single-precision rounding, so we need to provide some additional emulation

(require math/bigfloat
         math/flonum)
(provide float32?
         ->float32
         bigfloat->float32
         float32->ordinal
         ordinal->float32
         fl32+
         fl32-
         fl32*
         fl32/)

(module hairy racket/base
  (require (only-in math/private/bigfloat/mpfr get-mpfr-fun _mpfr-pointer _rnd_t bf-rounding-mode))
  (require ffi/unsafe)
  (provide bigfloat->float32)
  (define mpfr-get-flt (get-mpfr-fun 'mpfr_get_flt (_fun _mpfr-pointer _rnd_t -> _float)))
  (define (bigfloat->float32 x)
    (mpfr-get-flt x (bf-rounding-mode))))
(require (submod "." hairy))

(module+ test
  (require rackunit))

(define float32? flonum?)

(define (->float32 x)
  (flsingle (exact->inexact x)))

(define (float32->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f #f))

(define (float32->ordinal x)
  (if (negative? x)
      (- (float32->bit-field (- x)))
      (float32->bit-field (abs x))))

(define (bit-field->float32 x)
  (->float32 (floating-point-bytes->real (integer->integer-bytes x 4 #f #f) #f)))

(define (ordinal->float32 x)
  (if (negative? x)
      (- (bit-field->float32 (- x)))
      (bit-field->float32 x)))

(define (float32-step x n)
  (ordinal->float32 (+ (float32->ordinal x) n)))

(define fl32+ (compose ->float32 +))
(define fl32- (compose ->float32 -))
(define fl32* (compose ->float32 *))
(define fl32/ (compose ->float32 /))

(module+ test
  (check-equal? (fl32+ 1.0 2.0) (->float32 3.0))
  (check-equal? (fl32- 1.0 2.0) (->float32 -1.0))
  (check-equal? (fl32* 1.0 2.0) (->float32 2.0))
  (check-equal? (fl32/ 1.0 2.0) (->float32 0.5)))
