#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         (only-in "batch.rkt" batch-nodes))

(provide expr?
         expr<?
         all-subexpressions
         ops-in-expr
         spec-prog?
         impl-prog?
         repr-of
         repr-of-node
         location-do
         location-get
         free-variables
         replace-expression
         replace-vars)

;; Programs are just lisp lists plus atoms

(define expr? (or/c list? symbol? boolean? real? literal? approx?))

;; Returns repr name
;; Fast version does not recurse into functions applications
(define (repr-of expr ctx)
  (match expr
    [(literal val precision) (get-representation precision)]
    [(? variable?) (context-lookup ctx expr)]
    [(approx _ impl) (repr-of impl ctx)]
    [(list 'if cond ift iff) (repr-of ift ctx)]
    [(list op args ...) (impl-info op 'otype)]))

; Index inside (batch-nodes batch) -> type
(define (repr-of-node batch idx ctx)
  (define node (vector-ref (batch-nodes batch) idx))
  (match node
    [(literal val precision) (get-representation precision)]
    [(? variable?) (context-lookup ctx node)]
    [(approx _ impl) (repr-of-node batch impl ctx)]
    [(hole precision spec) (get-representation precision)]
    [(list 'if cond ift iff) (repr-of-node batch ift ctx)]
    [(list op args ...) (impl-info op 'otype)]))

(define (all-subexpressions expr #:reverse? [reverse? #f])
  (define subexprs
    (reap [sow]
          (let loop ([expr expr])
            (sow expr)
            (match expr
              [(? number?) (void)]
              [(? literal?) (void)]
              [(? variable?) (void)]
              [(approx _ impl) (loop impl)]
              [`(if ,c ,t ,f)
               (loop c)
               (loop t)
               (loop f)]
              [(list _ args ...)
               (for ([arg args])
                 (loop arg))]))))
  (remove-duplicates (if reverse?
                         (reverse subexprs)
                         subexprs)))

(define (ops-in-expr expr)
  (remove-duplicates (filter-map (lambda (e) (and (pair? e) (first e))) (all-subexpressions expr))))

;; Is the expression in LSpec (real expressions)?
(define (spec-prog? expr)
  (match expr
    [(? symbol?) #t]
    [(? number?) #t]
    [(list 'if cond ift iff) (and (spec-prog? cond) (spec-prog? ift) (spec-prog? iff))]
    [(list (? operator-exists?) args ...) (andmap spec-prog? args)]
    [_ #f]))

;; Is the expression in LImpl (floating-point implementations)?
(define (impl-prog? expr)
  (match expr
    [(? symbol?) #t]
    [(? literal?) #t]
    [(approx spec impl) (and (spec-prog? spec) (impl-prog? impl))]
    [(list 'if cond ift iff) (and (impl-prog? cond) (impl-prog? ift) (impl-prog? iff))]
    [(list (? impl-exists?) args ...) (andmap impl-prog? args)]
    [_ #f]))

;; Total order on expressions

(define (expr-cmp a b)
  (match* (a b)
    [((? list?) (? list?))
     (define len-a (length a))
     (define len-b (length b))
     (cond
       [(< len-a len-b) -1]
       [(> len-a len-b) 1]
       [else
        (let loop ([a a]
                   [b b])
          (if (null? a)
              0
              (let ([cmp (expr-cmp (car a) (car b))])
                (if (zero? cmp)
                    (loop (cdr a) (cdr b))
                    cmp))))])]
    [((? list?) _) 1]
    [(_ (? list?)) -1]
    [((? approx?) (? approx?))
     (define cmp-spec (expr-cmp (approx-spec a) (approx-spec b)))
     (if (zero? cmp-spec)
         (expr-cmp (approx-impl a) (approx-impl b))
         cmp-spec)]
    [((? approx?) _) 1]
    [(_ (? approx?)) -1]
    [((? symbol?) (? symbol?))
     (cond
       [(symbol<? a b) -1]
       [(symbol=? a b) 0]
       [else 1])]
    [((? symbol?) _) 1]
    [(_ (? symbol?)) -1]
    ;; Need both cases because `reduce` uses plain numbers
    [((or (? literal? (app literal-value a)) (? number? a)) (or (? literal? (app literal-value b))
                                                                (? number? b)))
     (cond
       [(< a b) -1]
       [(= a b) 0]
       [else 1])]))

(define (expr<? a b)
  (< (expr-cmp a b) 0))

;; Converting constants

(define (free-variables prog)
  (match prog
    [(? literal?) '()]
    [(? number?) '()]
    [(? variable?) (list prog)]
    [(approx _ impl) (free-variables impl)]
    [(list _ args ...) (remove-duplicates (append-map free-variables args))]))

(define (replace-vars dict expr)
  (let loop ([expr expr])
    (match expr
      [(? literal?) expr]
      [(? number?) expr]
      [(? symbol?) (dict-ref dict expr expr)]
      [(approx impl spec) (approx (loop impl) (loop spec))]
      [(list op args ...) (cons op (map loop args))])))

(define location? (listof natural-number/c))

(define/contract (location-do loc prog f)
  (-> location? expr? (-> expr? expr?) expr?)
  (define (invalid! where loc)
    (error 'location-do "invalid location `~a` for `~a` in `~a`" loc where prog))

  (let loop ([prog prog]
             [loc loc])
    (match* (prog loc)
      [(_ (? null?)) (f prog)]
      [((or (? literal?) (? number?) (? symbol?)) _) (invalid! prog loc)]
      [((approx spec impl) (cons idx rest)) ; approx nodes
       (case idx
         [(1) (approx (loop spec rest) impl)]
         [(2) (approx spec (loop impl rest))]
         [else (invalid! prog loc)])]
      [((list op args ...) (cons idx rest)) ; operator
       (let seek ([elts (cons op args)]
                  [idx idx])
         (cond
           [(= idx 0) (cons (loop (car elts) rest) (cdr elts))]
           [(null? elts) (invalid! prog loc)]
           [else (cons (car elts) (seek (cdr elts) (sub1 idx)))]))]
      [(_ _) (invalid! prog loc)])))

(define/contract (location-get loc prog)
  (-> location? expr? expr?)
  ; Clever continuation usage to early-return
  (let/ec return
    (location-do loc prog return)))

(define/contract (replace-expression expr from to)
  (-> expr? expr? expr? expr?)
  (let loop ([expr expr])
    (match expr
      [(== from) to]
      [(? number?) expr]
      [(? literal?) expr]
      [(? symbol?) expr]
      [(approx spec impl) (approx (loop spec) (loop impl))]
      [(list op args ...) (cons op (map loop args))])))

(module+ test
  (require rackunit)
  (check-equal? (replace-expression '(- x (sin x)) 'x 1) '(- 1 (sin 1)))

  (check-equal? (replace-expression '(/ (cos (* 2 x))
                                        (* (pow cos 2) (* (fabs (* sin x)) (fabs (* sin x)))))
                                    'cos
                                    '(/ 1 cos))
                '(/ (cos (* 2 x)) (* (pow (/ 1 cos) 2) (* (fabs (* sin x)) (fabs (* sin x)))))))
