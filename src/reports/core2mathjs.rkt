#lang racket

(require fpbench
         fpbench/src/fpcore-visitor)
(provide core->mathjs
         expr->mathjs
         mathjs-supported)

(define mathjs-supported
  (supported-list (negate (curry set-member?
                                 '(while while* array dim size ref for for* tensor tensor*)))
                  (curry set-member? '(PI E TRUE FALSE))
                  (const #t)
                  ieee754-rounding-modes
                  #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;; utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fix-name name)
  (string-join (for/list ([char (~a name)])
                 (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
                     (string char)
                     (format "_~a_" (char->integer char))))
               ""))

(define infix-ops '(+ - * / == != < > <= >= not and or))

;;;;;;;;;;;;;;;;;;;;;;;;;;; compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (constant->mathjs expr ctx)
  (match expr
    ['E "E"]
    ['PI "Math.PI"]
    ['TRUE "true"]
    ['FALSE "false"]
    [(? hex?) ((compose ~a real->double-flonum hex->racket) expr)]
    [(? number?) (~a (real->double-flonum expr))]
    [(? symbol?) (~a expr)]))

(define (operator->mathjs op args ctx)
  (define format-arg (compose trim-infix-parens ~a))
  (format "~a(~a)" op (string-join (map format-arg args) ", ")))

(define (compile-infix-operator op args ctx)
  (match (cons op args)
    [(list '- a) (format "(- ~a)" a)]
    [(list 'not a) (format "!~a" a)]
    [(list (or '== '!= '< '> '<= '>=)) (constant->mathjs 'TRUE ctx)]
    ; binary arithmetic
    [(list (or '+ '- '* '/) a b) (format "(~a ~a ~a)" a op b)]
    [(list (or '== '< '> '<= '>=) arg args ...)
     (format "(~a)"
             (string-join (for/list ([a (cons arg args)]
                                     [b args])
                            (format "~a ~a ~a" a op b))
                          " && "))]
    [(list '!= args ...)
     (format "(~a)"
             (string-join (let loop ([args args])
                            (if (null? args)
                                '()
                                (append (for/list ([b (cdr args)])
                                          (format "~a != ~a" (car args) b))
                                        (loop (cdr args)))))
                          " && "))]
    [(list 'and a ...) (format "(~a)" (string-join (map ~a a) " && "))]
    [(list 'or a ...) (format "(~a)" (string-join (map ~a a) " || "))]))

(define (compile-operator op args ctx)
  (if (set-member? infix-ops op)
      (compile-infix-operator op args ctx)
      (operator->mathjs op args ctx)))

(define (visit-if/mathjs vtor cond ift iff #:ctx ctx)
  (format "(~a ? ~a : ~a)"
          (visit/ctx vtor cond ctx)
          (visit/ctx vtor ift ctx)
          (visit/ctx vtor iff ctx)))

(define (visit-let_/mathjs vtor let_ vars vals body #:ctx ctx)
  (define ctx*
    (for/fold ([ctx* ctx])
              ([var (in-list vars)]
               [val (in-list vals)])
      (define val*
        (visit/ctx vtor
                   val
                   (match let_
                     ['let ctx]
                     ['let* ctx*])))
      (define-values (name-ctx name) (ctx-unique-name ctx* var #f)) ; don't use precision
      (printf "~a = ~a;\n" name (trim-infix-parens val*))
      name-ctx))
  (visit/ctx vtor body ctx*))

(define (visit-!/mathjs vtor props body #:ctx ctx)
  ; TODO: Ignoring the right thing?
  (define ctx* (ctx-update-props ctx props))
  (visit/ctx vtor body ctx*))

(define (visit_op/mathjs vtor op args #:ctx ctx)
  (define args*
    (for/list ([arg args])
      (visit/ctx vtor arg ctx)))
  (compile-operator op args* ctx))

(define (visit-digits/mathjs vtor m e b #:ctx ctx)
  (visit/ctx vtor (digits->number m e b) ctx))

(define (visit-number/mathjs vtor x #:ctx ctx)
  (constant->mathjs x ctx))

(define (visit-constant/mathjs vtor x #:ctx ctx)
  (constant->mathjs x ctx))

(define (visit-symbol/mathjs vtor x #:ctx ctx)
  (ctx-lookup-name ctx x))

(define-expr-visitor default-compiler-visitor
                     mathjs-visitor
                     [visit-if visit-if/mathjs]
                     [visit-let_ visit-let_/mathjs]
                     [visit-! visit-!/mathjs]
                     [visit-op_ visit_op/mathjs]
                     [visit-digits visit-digits/mathjs]
                     [visit-number visit-number/mathjs]
                     [visit-constant visit-constant/mathjs]
                     [visit-symbol visit-symbol/mathjs])

;;;;;;;;;;;;;;;;;;;;;;;;;;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expr->mathjs prog [name ""])
  (parameterize ([*gensym-used-names* (mutable-set)]
                 [*gensym-collisions* 1]
                 [*gensym-fix-name* fix-name])
    ; make compiler context
    (define ctx
      (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven))))

    ; translate
    (define p (open-output-string))
    (define-values (body* ret)
      (parameterize ([current-output-port p])
        (define o (visit/ctx mathjs-visitor prog ctx))
        (values (get-output-string p) (trim-infix-parens o))))

    (format "~a~a" body* ret)))

(define (core->mathjs prog [name ""])
  (parameterize ([*gensym-used-names* (mutable-set)]
                 [*gensym-collisions* 1]
                 [*gensym-fix-name* fix-name])
    ; decompose FPCore
    (define-values (args props body)
      (match prog
        [(list 'FPCore (list args ...) props ... body) (values args props body)]
        [(list 'FPCore name (list args ...) props ... body) (values args props body)]))

    ; make compiler context
    (define ctx
      (ctx-update-props (make-compiler-ctx) (append '(:precision binary64 :round nearestEven) props)))

    ; handle variable annotations
    (define arg-names
      (for/list ([var args])
        (match var
          [(list '! props ... name) (fix-name (~a name))]
          [_ (fix-name (~a var))])))

    ; translate (body only)
    (define p (open-output-string))
    (define-values (body* ret)
      (parameterize ([current-output-port p])
        (define o (visit/ctx mathjs-visitor body ctx))
        (values (get-output-string p) (trim-infix-parens o))))

    (format "~a~a" body* ret)))
