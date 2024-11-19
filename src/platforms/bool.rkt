#lang racket

;; Builtin boolean plugin

(require "runtime/utils.rkt")

;; Do not run this file with `raco test`
(module test racket/base
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-representation (bool bool boolean?)
                       identity
                       identity
                       (λ (x) (= x 0))
                       (λ (x) (if x 1 0))
                       1
                       (const #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constants bool [TRUE TRUE.bool true] [FALSE FALSE.bool false])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and-fn . as)
  (andmap identity as))
(define (or-fn . as)
  (ormap identity as))

(define-operator-impl (not.bool [x : bool]) bool #:spec (not x) #:fl not)

(define-operator-impl (and.bool [x : bool] [y : bool]) bool #:spec (and x y) #:fl and-fn)

(define-operator-impl (or.bool [x : bool] [y : bool]) bool #:spec (or x y) #:fl or-fn)