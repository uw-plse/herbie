#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "types.rkt"
         "platform.rkt"
         "syntax.rkt")
(provide assert-program-typed!)

(define (assert-program-typed! stx)
  (define-values (vars props body)
    (match (syntax-e stx)
      [(list (app syntax-e 'FPCore) _ (app syntax-e (list vars ...)) props ... body)
       (values vars props body)]
      [(list (app syntax-e 'FPCore) (app syntax-e (list vars ...)) props ... body)
       (values vars props body)]))

  (define default-dict `((:precision . ,(*default-precision*))))
  (define prop-dict (apply dict-set* default-dict (map syntax->datum props)))
  (define prec (dict-ref prop-dict ':precision))

  (define-values (var-names var-precs)
    (for/lists (var-names var-precs)
               ([var (in-list vars)])
               (match (syntax->datum var)
                 [(list '! props ... name)
                  (define prop-dict (props->dict props))
                  (define arg-prec (dict-ref prop-dict ':precision prec))
                  (values name arg-prec)]
                 [(? symbol? name) (values name prec)])))

  (define ctx (context var-names (get-representation prec) (map get-representation var-precs)))
  (assert-expression-type! body prop-dict ctx))

(define (assert-expression-type! stx props ctx)
  (define errs '())
  (define (error! stx fmt . args)
    (define args*
      (for/list ([arg (in-list args)])
        (match arg
          [(? representation?) (representation-name arg)]
          [_ arg])))
    (set! errs (cons (cons stx (apply format fmt args*)) errs)))

  (define repr (expression->type stx props ctx error!))
  (unless (equal? repr (context-repr ctx))
    (error! stx "Expected program of type ~a, got type ~a" (context-repr ctx) repr))

  (unless (null? errs)
    (raise-herbie-syntax-error "Program has type errors" #:locations errs)))

(define (application->string op types)
  (format "(~a ~a)"
          op
          (string-join (for/list ([t types])
                         (if t
                             (format "<~a>" (representation-name t))
                             "<?>"))
                       " ")))

(define (expression->type stx prop-dict ctx error!)
  (let loop ([stx stx]
             [prop-dict prop-dict]
             [ctx ctx])
    (match stx
      [#`,(? number?) (get-representation (dict-ref prop-dict ':precision))]
      [#`,(? variable? x) (context-lookup ctx x)]
      [#`,(? constant-operator? op)
       (match (get-fpcore-impl op prop-dict '())
         [#f ; no implementation found
          (error! stx "No implementation of `~a` in platform for context `~a`" op prop-dict)
          (get-representation (dict-ref prop-dict ':precision))]
         [impl (impl-info impl 'otype)])]
      [#`(let ([,ids #,exprs] ...) #,body)
       (define ctx*
         (for/fold ([ctx* ctx])
                   ([id (in-list ids)]
                    [expr (in-list exprs)])
           (context-extend ctx* id (loop expr prop-dict ctx))))
       (loop body prop-dict ctx*)]
      [#`(let* ([,ids #,exprs] ...) #,body)
       (define ctx*
         (for/fold ([ctx* ctx])
                   ([id (in-list ids)]
                    [expr (in-list exprs)])
           (context-extend ctx* id (loop expr prop-dict ctx*))))
       (loop body prop-dict ctx*)]
      [#`(if #,branch #,ifstmt #,elsestmt)
       (define cond-ctx (struct-copy context ctx [repr (get-representation 'bool)]))
       (define cond-repr (loop branch prop-dict cond-ctx))
       (unless (equal? (representation-type cond-repr) 'bool)
         (error! stx "If statement has non-boolean type ~a for branch" cond-repr))
       (define ift-repr (loop ifstmt prop-dict ctx))
       (define iff-repr (loop elsestmt prop-dict ctx))
       (unless (equal? ift-repr iff-repr)
         (error! stx "If statement has different types for if (~a) and else (~a)" ift-repr iff-repr))
       ift-repr]
      [#`(! #,props ... #,body) (loop body (apply dict-set prop-dict (map syntax->datum props)) ctx)]
      [#`(,(? (curry hash-has-key? (*functions*)) fname) #,args ...)
       ; TODO: inline functions expect uniform types, this is clearly wrong
       (match-define (list vars prec _) (hash-ref (*functions*) fname))
       (define repr (get-representation prec))
       (define ireprs (map (lambda (arg) (loop arg prop-dict ctx)) args))
       (define expected (map (const repr) vars))
       (unless (andmap equal? ireprs expected)
         (error! stx
                 "Invalid arguments to ~a; expects ~a but got ~a"
                 fname
                 fname
                 (application->string fname expected)
                 (application->string fname ireprs)))
       repr]
      [#`(cast #,arg)
       (define irepr (loop arg prop-dict ctx))
       (define repr (get-representation (dict-ref prop-dict ':precision)))
       (cond
         [(equal? irepr repr) repr]
         [else
          (match (get-fpcore-impl 'cast prop-dict (list irepr))
            [#f ; no implementation found
             (error! stx
                     "No implementation of `~a` in platform for context `~a`"
                     (application->string 'cast (list irepr))
                     prop-dict)
             (get-representation (dict-ref prop-dict ':precision))]
            [impl (impl-info impl 'otype)])])]
      [#`(,(? symbol? op) #,args ...)
       (define ireprs (map (lambda (arg) (loop arg prop-dict ctx)) args))
       (match (get-fpcore-impl op prop-dict ireprs)
         [#f ; no implementation found
          (error! stx
                  "No implementation of `~a` in platform for context `~a`"
                  (application->string op ireprs)
                  prop-dict)
          (get-representation (dict-ref prop-dict ':precision))]
         [impl (impl-info impl 'otype)])])))

(module+ test
  (require rackunit)
  (require "load-plugin.rkt")
  (load-herbie-builtins)

  (define (fail! stx msg . args)
    (error (apply format msg args) stx))

  (define (check-types env-type rtype expr #:env [env '()])
    (define ctx (context (map car env) env-type (map cdr env)))
    (define repr (expression->type expr (repr->prop env-type) ctx fail!))
    (check-equal? repr rtype))

  (define (check-fails type expr #:env [env '()])
    (define fail? #f)
    (define ctx (context (map car env) type (map cdr env)))
    (expression->type expr (repr->prop type) ctx (lambda _ (set! fail? #t)))
    (check-true fail?))

  (define <bool> (get-representation 'bool))
  (define <b64> (get-representation 'binary64))

  (check-types <b64> <b64> #'4)
  (check-types <b64> <b64> #'x #:env `((x . ,<b64>)))
  (check-types <b64> <b64> #'(acos x) #:env `((x . ,<b64>)))
  (check-fails <b64> #'(acos x) #:env `((x . ,<bool>)))
  (check-types <b64> <bool> #'(and a b) #:env `((a . ,<bool>) (b . ,<bool>)))
  (check-types <b64> <b64> #'(if (== a 1) 1 0) #:env `((a . ,<b64>)))
  (check-fails <b64> #'(if (== a 1) 1 0) #:env `((a . ,<bool>)))
  (check-types <b64> <bool> #'(let ([a 1]) TRUE))
  (check-fails <b64> #'(if (== a 1) 1 TRUE) #:env `((a . ,<b64>)))
  (check-types <b64> <b64> #'(let ([a 1]) a) #:env `((a . ,<bool>))))
