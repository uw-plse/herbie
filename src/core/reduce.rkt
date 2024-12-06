#lang racket

(require "../utils/common.rkt"
         "programs.rkt"
         "../syntax/syntax.rkt")
(provide simplify)

;; Cancellation's goal is to cancel (additively or multiplicatively) like terms.
;; It uses commutativity, identities, inverses, associativity,
;; distributativity, and function inverses.

(define/reset simplify-cache (make-hash))

(define/reset simplify-node-cache (make-hash))

;; This is a transcription of egg-herbie/src/math.rs, lines 97-149
(define (eval-application op . args)
  (define exact-value? (conjoin number? exact?))
  (match (cons op args)
    [(list '+ (? exact-value? as) ...) (apply + as)]
    [(list '- (? exact-value? as) ...) (apply - as)]
    [(list '* (? exact-value? as) ...) (apply * as)]
    [(list '/ (? exact-value? num) (? exact-value? den)) (and (not (zero? den)) (/ num den))]
    [(list 'neg (? exact-value? arg)) (- arg)]
    [(list 'pow (? exact-value? a) (? exact-value? b))
     (cond
       [(and (zero? b) (not (zero? a))) 1]
       [(and (zero? a) (positive? b)) 0]
       [(and (not (zero? a)) (integer? b)) (expt a b)]
       [else #f])]
    [(list 'sqrt (? exact-value? a))
     (let ([s1 (sqrt (numerator a))]
           [s2 (sqrt (denominator a))])
       (and (real? s1) (real? s2) (exact? s1) (exact? s2) (/ s1 s2)))]
    [(list 'cbrt (? exact-value? a))
     (define inexact-num (inexact->exact (expt (numerator a) 1/3)))
     (define inexact-den (inexact->exact (expt (denominator a) 1/3)))
     (and (real? inexact-num)
          (real? inexact-den)
          (= (expt inexact-num 3) (numerator a))
          (= (expt inexact-den 3) (denominator a))
          (/ inexact-num inexact-den))]
    [(list 'fabs (? exact-value? a)) (abs a)]
    [(list 'floor (? exact-value? a)) (floor a)]
    [(list 'ceil (? exact-value? a)) (ceiling a)]
    [(list 'round (? exact-value? a)) (round a)]
    ;; Added
    [(list 'exp 0) 1]
    [(list 'log 1) 0]
    [_ #f]))

(module+ test
  (require rackunit)

  (check-equal? (eval-application '+ 1 1) 2)
  (check-equal? (eval-application '+) 0)
  (check-equal? (eval-application '/ 1 0) #f) ; Not valid
  (check-equal? (eval-application 'cbrt 1) 1)
  (check-equal? (eval-application 'log 1) 0)
  (check-equal? (eval-application 'exp 2) #f)) ; Not exact

(define (simplify expr)
  (hash-ref! (simplify-cache) expr (λ () (simplify* expr))))

(define (simplify* expr)
  (match expr
    [(? number?) expr]
    [(? symbol?) expr]
    ; conversion (e.g. posit16->f64)
    [(list (? cast-impl? op) body) (list op (simplify body))]
    [`(,(and (or '+ '- '*) op) ,args ...) ; v-ary
     (define args* (map simplify args))
     (define val (apply eval-application op args*))
     (or val (simplify-node (list* op args*)))]
    [`(,op ,args ...)
     (define args* (map simplify args))
     (define val (apply eval-application op args*))
     (or val (simplify-node (list* op args*)))]))

(define (simplify-evaluation expr)
  (match expr
    ['(sin 0) 0]
    ['(cos 0) 1]
    ['(sin (PI)) 0]
    ['(cos (PI)) -1]
    ['(exp 1) '(E)]
    ['(tan 0) 0]
    ['(sinh 0) 0]
    ['(log (E)) 1]
    ['(exp 0) 1]
    ['(tan (PI)) 0]
    ['(cosh 0) 1]
    ['(cos (/ (PI) 6)) '(/ (sqrt 3) 2)]
    ['(tan (/ (PI) 3)) '(sqrt 3)]
    ['(tan (/ (PI) 4)) 1]
    ['(cos (/ (PI) 2)) 0]
    ['(tan (/ (PI) 6)) '(/ 1 (sqrt 3))]
    ['(sin (/ (PI) 3)) '(/ (sqrt 3) 2)]
    ['(sin (/ (PI) 6)) 1/2]
    ['(sin (/ (PI) 4)) '(/ (sqrt 2) 2)]
    ['(sin (/ (PI) 2)) 1]
    ['(cos (/ (PI) 3)) 1/2]
    ['(cos (/ (PI) 4)) '(/ (sqrt 2) 2)]
    [_ expr]))

(define (simplify-inverses expr)
  (match expr
    [`(tanh (atanh ,x)) x]
    [`(cosh (acosh ,x)) x]
    [`(sinh (asinh ,x)) x]
    [`(acos (cos ,x)) x]
    [`(asin (sin ,x)) x]
    [`(atan (tan ,x)) x]
    [`(tan (atan ,x)) x]
    [`(cos (acos ,x)) x]
    [`(sin (asin ,x)) x]
    [`(pow ,x 1) x]
    [`(log (exp ,x)) x]
    [`(exp (log ,x)) x]
    [`(cbrt (pow ,x 3)) x]
    [`(pow (cbrt ,x) 3) x]
    [_ expr]))

(define (simplify-node expr)
  (hash-ref! (simplify-node-cache) expr (λ () (simplify-node* expr))))

(define (simplify-node* expr)
  (match (simplify-evaluation expr)
    [(? number?) expr]
    [(? variable?) expr]
    [(or `(+ ,_ ...) `(- ,_ ...) `(neg ,_))
     (make-addition-node (combine-aterms (gather-additive-terms expr)))]
    [(or `(* ,_ ...) `(/ ,_ ...) `(sqrt ,_) `(cbrt ,_) `(pow ,_ ,_))
     (make-multiplication-node (combine-mterms (gather-multiplicative-terms expr)))]
    [`(exp (* ,c (log ,x))) (simplify-node* `(pow ,x ,c))]
    [else (simplify-inverses expr)]))

(define (negate-term term)
  (cons (- (car term)) (cdr term)))

(define (gather-additive-terms expr #:label [label #f])
  (define (recurse subexpr #:label [label #f])
    (gather-additive-terms subexpr #:label label))

  (let ([label (or label expr)])
    (match expr
      [(? number?) `((,expr 1))]
      [(? variable?) `((1 ,expr))]
      [`(+ ,args ...) (append-map recurse args)]
      [`(neg ,arg) (map negate-term (recurse arg))]
      [`(- ,arg ,args ...) (append (recurse arg) (map negate-term (append-map recurse args)))]

      ; Prevent fall-through to the next case
      [`(/ ,arg) `((1 ,expr))]
      [`(/ ,arg ,args ...)
       (for/list ([term (recurse arg)])
         (list* (car term) (simplify-node (list* '/ (cadr term) args)) (cons label (cddr term))))]

      [`(pow ,arg 1) `((1 1))]
      [else `((1 ,expr))])))

(define (gather-multiplicative-terms expr)
  (match expr
    [(? number?) `(,expr)]
    ['NAN `(NAN)]
    [(? symbol?) `(1 (1 . ,expr))]
    [`(neg ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (if (eq? (car terms) 'NAN)
           '(NAN)
           (cons (- (car terms)) (cdr terms))))]
    [`(* ,args ...)
     (let ([terms (map gather-multiplicative-terms args)])
       (if (ormap (curry eq? 'NAN) (map car terms))
           '(NAN)
           (cons (apply * (map car terms)) (apply append (map cdr terms)))))]
    [`(/ ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (cons (if (member (car terms) '(0 NAN))
                 'NAN
                 (/ (car terms)))
             (map negate-term (cdr terms))))]
    [`(/ ,arg ,args ...)
     (let ([num (gather-multiplicative-terms arg)]
           [dens (map gather-multiplicative-terms args)])
       (cons (if (or (eq? (car num) 'NAN) (ormap (compose (curryr member '(0 NAN)) car) dens))
                 'NAN
                 (apply / (car num) (map car dens)))
             (append (cdr num) (map negate-term (append-map cdr dens)))))]
    [`(sqrt ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (define exact-sqrt
         (match (car terms)
           ['NAN 'NAN]
           [x (eval-application 'sqrt x)]))
       (if exact-sqrt
           (cons exact-sqrt
                 (for/list ([term (cdr terms)])
                   (cons (/ (car term) 2) (cdr term))))
           (list* 1
                  (cons 1 `(sqrt ,(car terms)))
                  (for/list ([term (cdr terms)])
                    (cons (/ (car term) 2) (cdr term))))))]
    [`(cbrt ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (define exact-cbrt
         (match (car terms)
           ['NAN 'NAN]
           [x (eval-application 'cbrt (car terms))]))
       (if exact-cbrt
           (cons exact-cbrt
                 (for/list ([term (cdr terms)])
                   (cons (/ (car term) 3) (cdr term))))
           (list* 1
                  (cons 1 `(cbrt ,(car terms)))
                  (for/list ([term (cdr terms)])
                    (cons (/ (car term) 3) (cdr term))))))]
    [`(pow ,arg 0) '(1)]
    [`(pow ,arg ,(? real? a))
     (let ([terms (gather-multiplicative-terms arg)])
       (define exact-pow
         (match (car terms)
           ['NAN 'NAN]
           [x (eval-application 'pow (car terms) a)]))
       (if exact-pow
           (cons exact-pow
                 (for/list ([term (cdr terms)])
                   (cons (* a (car term)) (cdr term))))
           (list* 1
                  (cons a (car terms))
                  (for/list ([term (cdr terms)])
                    (cons (* a (car term)) (cdr term))))))]
    [else `(1 (1 . ,expr))]))

(define (combine-aterms terms)
  (let ([h (make-hash)])
    (for ([term terms])
      (let ([sum (hash-ref! h (cadr term) (λ () 0))]) (hash-set! h (cadr term) (+ (car term) sum))))
    (sort (reap [sow]
                (hash-for-each h
                               (λ (k v)
                                 (when (not (= v 0))
                                   (sow (cons v k))))))
          expr<?
          #:key cdr)))

(define (combine-mterms terms)
  (cons (car terms)
        (let ([h (make-hash)])
          (for ([term (cdr terms)])
            (let ([sum (hash-ref! h (cdr term) (λ () 0))])
              (hash-set! h (cdr term) (+ (car term) sum))))
          (sort (reap [sow]
                      (hash-for-each h
                                     (λ (k v)
                                       (when (not (= v 0))
                                         (sow (cons v k))))))
                expr<?
                #:key cdr))))

(define (aterm->expr term)
  (match term
    [`(1 . ,x) x]
    [`(,x . 1) x]
    [`(-1 . ,x) `(neg ,x)]
    [`(,coeff . ,x) `(* ,coeff ,x)]))

(define (make-addition-node terms)
  (let-values ([(pos neg) (partition (λ (x) (and (real? (car x)) (positive? (car x)))) terms)])
    (cond
      [(and (null? pos) (null? neg)) 0]
      [(null? pos) `(neg ,(make-addition-node* (map negate-term neg)))]
      [(null? neg) (make-addition-node* pos)]
      [else `(- ,(make-addition-node* pos) ,(make-addition-node* (map negate-term neg)))])))

;; TODO : Use (- x y) when it is simpler
(define (make-addition-node* terms)
  (match terms
    ['() 0]
    [`(,term) (aterm->expr term)]
    [`(,term ,terms ...) `(+ ,(aterm->expr term) ,(make-addition-node terms))]))

(define (make-multiplication-node term)
  (match (cons (car term) (make-multiplication-subnode (cdr term)))
    [`(NAN . ,e) '(NAN)]
    [`(0 . ,e) 0]
    [`(1 . ()) 1]
    [`(1 . ,e) e]
    [`(,a . 1) a]
    [`(,a . (/ 1 ,denom)) `(/ ,a ,denom)]
    [`(,a . ()) a]
    [`(,a . ,e) `(* ,a ,e)]))

(define (make-multiplication-subnode terms)
  (make-multiplication-subsubsubnode
   (for/list ([rootgroup (group-by (compose denominator car) terms)])
     (let* ([denom (denominator (caar rootgroup))]
            [newterms (map (λ (term) (cons (* (car term) denom) (cdr term))) rootgroup)])
       (cons 1 (mterm->expr (cons (/ 1 denom) (make-multiplication-subsubnode newterms))))))))

(define (make-multiplication-subsubnode terms)
  (let-values ([(pos neg) (partition (compose positive? car) terms)])
    (cond
      [(and (null? pos) (null? neg)) 1]
      [(null? pos) `(/ 1 ,(make-multiplication-subsubsubnode (map negate-term neg)))]
      [(null? neg) (make-multiplication-subsubsubnode pos)]
      [else
       `(/ ,(make-multiplication-subsubsubnode pos)
           ,(make-multiplication-subsubsubnode (map negate-term neg)))])))

(define (make-multiplication-subsubsubnode terms)
  (match terms
    ['() 1]
    [`(,term) (mterm->expr term)]
    [`(,term ,terms ...) `(* ,(mterm->expr term) ,(make-multiplication-subsubsubnode terms))]))

(define (mterm->expr term)
  (match term
    [`(1 . ,x) x]
    [`(-1 . ,x) `(/ 1 ,x)]
    [`(1/2 . ,x) `(sqrt ,x)]
    [`(-1/2 . ,x) `(/ 1 (sqrt ,x))]
    [`(1/3 . ,x) `(cbrt ,x)]
    [`(-1/3 . ,x) `(/ 1 (cbrt ,x))]
    [`(,power . ,x) `(pow ,x ,power)]))
