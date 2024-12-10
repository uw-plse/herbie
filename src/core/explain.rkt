#lang racket

(require "points.rkt"
         "programs.rkt"
         "../utils/float.rkt"
         "logfloat.rkt"
         "compiler.rkt"
         "sampling.rkt"
         "../syntax/sugar.rkt")

(provide explain *condthres*)

(define (take-n n lst)
  (match lst
    ['() '()]
    [(cons x xs)
     (if (= n 0)
         '()
         (cons x (take-n (- n 1) xs)))]))

(define (constant? expr)
  (cond
    [(list? expr) (andmap constant? (rest expr))]
    [(symbol? expr) #f]
    [else #t]))

(define *condthres* (make-parameter 64))

(define (compile-expr expr ctx)
  (define compiled-lf (compile-lf (expr->lf expr) ctx))
  (define compiled-fl (compile-prog expr ctx))
  (define compiled-real (eval-progs-real (list (prog->spec expr)) (list ctx)))
  (values compiled-lf compiled-fl compiled-real))

(define (predict-errors-eftsan pctx ctx expr compiled-lf compiled-fl compiled-real)
  (define tp 0)
  (define fp 0)
  (define fn 0)
  (define tn 0)

  (for ([(pt _) (in-pcontext pctx)])
    (define lf-answer (apply compiled-lf (map lf pt)))
    (define fl-answer (apply compiled-fl pt))
    (define real-answer (car (apply compiled-real pt)))
    (define diff (ulp-difference (logfloat-r1 lf-answer) fl-answer (repr-of expr ctx)))
    (define rdiff (ulp-difference real-answer fl-answer (repr-of expr ctx)))
    (cond
      [(and (> diff (*condthres*)) (> rdiff 16)) (set! tp (+ tp 1))]
      [(and (> diff (*condthres*)) (<= rdiff 16)) (set! fp (+ fp 1))]
      [(and (<= diff (*condthres*)) (> rdiff 16))
       (set! fn (+ fn 1))]
      [(and (<= diff (*condthres*)) (<= rdiff 16)) (set! tn (+ tn 1))]
      ))
  (list tp fn fp tn))

(define (explain expr ctx pctx)
  (define-values (compiled-lf compiled-fl compiled-real) (compile-expr expr ctx))
  (predict-errors-eftsan pctx ctx expr compiled-lf compiled-fl compiled-real))
