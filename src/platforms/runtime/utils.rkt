#lang racket

;; Common definitions for the builtin plugins

(require "../../plugin.rkt")
(provide (all-defined-out)
         (all-from-out "../../plugin.rkt"))

(define (shift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (fn (- x shift-val))))

(define (unshift bits fn)
  (define shift-val (expt 2 bits))
  (λ (x) (+ (fn x) shift-val)))

(define (sym-append . args)
  (define strs (map ~s args))
  (string->symbol (apply string-append strs)))

(define-syntax-rule (define-constants repr [name impl-name value] ...)
  (begin
    (define-operator-impl (impl-name)
                          repr
                          #:spec (name)
                          #:fl (const value)
                          #:fpcore (! :precision repr name)) ...))

(define-syntax-rule (define-comparator-impls repr [name impl-name impl-fn attrib ...] ...)
  (begin
    (define-operator-impl (impl-name [x : repr] [y : repr])
                          bool
                          #:spec (name x y)
                          #:fl impl-fn
                          attrib ...) ...))
