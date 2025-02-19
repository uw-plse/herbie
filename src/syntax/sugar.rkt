;; Expression conversions
;;
;; Herbie uses three expression languages.
;; All formats are S-expressions with variables, numbers, and applications.
;;
;; FPCore: the input/output language
;;
;; - standardized interchange format with other tools
;; - using loopless, tensorless subset
;; - operators denote real computations while rounding contexts decide format
;;
;;  <FPCore> ::= (FPCore (<var> ...) <props> ... <expr>)
;;             | (FPCore <id> (<var> ...) <props> ... <expr>)
;;
;;  <expr>   ::= (let ([<id> <expr>] ...) <expr>)
;;             | (let* ([<id> <expr>] ...) <expr>)
;;             | (if <expr> <expr> <expr>
;;             | (cast <expr>)
;;             | (! <props> ... <expr>)
;;             | (<op> <expr> ...)
;;             | <number>
;;             | <id>
;;
;;  <var>    ::= <id>
;;             | (! <props> ... <expr>)
;;
;; LImpl: the language of floating-point expressions
;;
;; - internal language, describes floating-point expressions
;; - unary minus represented by `neg`
;; - every operator is rounded
;; - let expressions are inlined
;; - numbers are rounded to a particular format
;;
;; <expr> ::= (if <expr> <expr> <expr>)
;;        ::= (<impl> <expr> ...)
;;        ::= (literal <number> <repr>)
;;        ::= <id>
;;
;; Every operator has a type signature where types are representations.
;; In practice, most operator implemenetations have uniform representations but
;; operations like casts are multi-format.
;;
;; LSpec: the language of mathematical formula
;;
;; - internal language, describes real-number formula
;; - unary minus represented by `neg`
;; - every operator is a real-number operation
;; - every operator is supported by Rival
;; - let expressions are inlined
;; - casts are mapped to the identity operation
;; - numbers are formatless
;;
;; <expr> ::= (if <expr> <expr> <expr>)
;;        ::= (<op> <expr> ...)
;;        ::= <number>
;;        ::= <id>
;;

#lang racket

(require "../core/programs.rkt"
         "../utils/common.rkt"
         "../utils/errors.rkt"
         "platform.rkt"
         "matcher.rkt"
         "syntax.rkt"
         "types.rkt")

(provide fpcore->prog
         prog->fpcore)

;; Expression pre-processing for normalizing expressions.
;; Used for conversion from FPCore to other IRs.
(define (expand-expr expr)
  (let loop ([expr expr]
             [env '()])
    (match expr
      ; empty let/let* expression
      [`(,(or 'let 'let*) () ,body) (loop body env)]
      ; let* expression
      [`(let* ([,var ,val]
               ,rest ...)
          ,body)
       (loop `(let ([,var ,val]) (let* ,rest ,body)) env)]
      ; let expression
      [`(let ([,vars ,vals] ...) ,body)
       (define env*
         (for/fold ([env* env])
                   ([var (in-list vars)]
                    [val (in-list vals)])
           (dict-set env* var (loop val env))))
       (loop body env*)]
      ; nullary expressions
      ['(and) '(TRUE)]
      ['(or) '(FALSE)]
      [`(,(or '+ '-)) 0]
      [`(,(or '* '/)) 1]
      ; unary expressions
      [`(,(or '+ '* 'and 'or) ,a) (loop a env)]
      [`(- ,a) `(neg ,(loop a env))]
      [`(/ ,a) `(/ 1 ,(loop a env))]
      ; expand arithmetic associativity
      [`(,(and (or '+ '- '* '/ 'and 'or) op) ,as ..2 ,b) `(,op ,(loop `(,op ,@as) env) ,(loop b env))]
      ; expand comparison associativity
      [`(,(and (or '< '<= '> '>= '=) op) ,as ...)
       (define as* (map (curryr loop env) as))
       (define out
         (for/fold ([out #f])
                   ([term as*]
                    [next (cdr as*)])
           (if out
               (list 'and out (list op term next))
               (list op term next))))
       (or out '(TRUE))]
      [`(!= ,as ...)
       (define as* (map (curryr loop env) as))
       (define out
         (for/fold ([out #f])
                   ([term as*]
                    [i (in-naturals)]
                    #:when true
                    [term2 as*]
                    [j (in-naturals)]
                    #:when (< i j))
           (if out
               (list 'and out (list '!= term term2))
               (list '!= term term2))))
       (or out '(TRUE))]
      ; function calls
      [(list (? (curry hash-has-key? (*functions*)) fname) args ...)
       (match-define (list vars _ body) (hash-ref (*functions*) fname))
       (define env*
         (for/fold ([env* '()])
                   ([var (in-list vars)]
                    [arg (in-list args)])
           (dict-set env* var (loop arg env))))
       (loop body env*)]
      ; applications
      [`(,op ,args ...) `(,op ,@(map (curryr loop env) args))]
      ; constants
      [(? constant-operator?) (list expr)]
      ; variables
      [(? symbol?) (dict-ref env expr expr)]
      ; other
      [_ expr])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FPCore -> LImpl

(define (assert-fpcore-impl op prop-dict ireprs)
  (or (get-fpcore-impl op prop-dict ireprs)
      (raise-herbie-missing-error
       "No implementation for `~a` under rounding context `~a` with types `~a`"
       op
       prop-dict
       (string-join (map (λ (r) (format "<~a>" (representation-name r))) ireprs) " "))))

;; Translates an FPCore operator application into
;; an LImpl operator application.
(define (fpcore->impl-app op prop-dict args ctx)
  (define ireprs (map (lambda (arg) (repr-of arg ctx)) args))
  (define impl (assert-fpcore-impl op prop-dict ireprs))
  (define vars (impl-info impl 'vars))
  (define pattern
    (match (impl-info impl 'fpcore)
      [(list '! _ ... body) body]
      [body body]))
  (define subst (pattern-match pattern (cons op args)))
  (pattern-substitute (cons impl vars) subst))

;; Translates from FPCore to an LImpl.
(define (fpcore->prog prog ctx)
  (let loop ([expr (expand-expr prog)]
             [prop-dict (repr->prop (context-repr ctx))])
    (match expr
      [(? number? n)
       (literal (match n
                  [(or +inf.0 -inf.0 +nan.0) expr]
                  [(? exact?) expr]
                  [_ (inexact->exact expr)])
                (dict-ref prop-dict ':precision))]
      [(? variable?) expr]
      [(list 'if cond ift iff)
       (define cond* (loop cond prop-dict))
       (define ift* (loop ift prop-dict))
       (define iff* (loop iff prop-dict))
       (list 'if cond* ift* iff*)]
      [(list '! props ... body)
       (loop body
             (if (not (null? props))
                 (apply dict-set* prop-dict props)
                 prop-dict))]
      [(list 'neg arg) ; non-standard but useful [TODO: remove]
       (define arg* (loop arg prop-dict))
       (fpcore->impl-app '- prop-dict (list arg*) ctx)]
      [(list 'cast arg) ; special case: unnecessary casts
       (define arg* (loop arg prop-dict))
       (define repr (get-representation (dict-ref prop-dict ':precision)))
       (if (equal? (repr-of arg* ctx) repr)
           arg*
           (fpcore->impl-app 'cast prop-dict (list arg*) ctx))]
      [(list op args ...)
       (define args* (map (lambda (arg) (loop arg prop-dict)) args))
       (fpcore->impl-app op prop-dict args* ctx)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LImpl -> FPCore
;; Translates from LImpl to an FPCore

;; TODO: this process uses a batch-like data structure
;; but _without_ deduplication since different use sites
;; of a particular subexpression may have different
;; parent rounding contexts. Would be nice to explore
;; if the batch data structure can be used.

;; Instruction vector index
(struct index (v) #:prefab)

;; Translates a literal (LImpl) to an FPCore expr
(define (literal->fpcore x)
  (match x
    [(literal -inf.0 _) '(- INFINITY)]
    [(literal +inf.0 _) 'INFINITY]
    [(literal v (or 'binary64 'binary32)) (exact->inexact v)]
    [(literal v _) v]))

;; Step 1.
;; Translates from LImpl to a series of let bindings such that each
;; local variable is bound once and used at most once. The result is an
;; instruction vector, representing the let bindings; the operator
;; implementation for each instruction, and the final "root" operation/literal.
;; Except for let-bound variables, the subexpressions are in FPCore.

(define (prog->let-exprs expr)
  (define instrs '())
  (define (push! impl node)
    (define id (length instrs))
    (set! instrs (cons (cons node impl) instrs))
    (index id))

  (define (munge expr #:root? [root? #f])
    (match expr
      [(? literal?) (literal->fpcore expr)]
      [(? symbol?) expr]
      [(approx _ impl) (munge impl)]
      [(list 'if cond ift iff) (list 'if (munge cond) (munge ift) (munge iff))]
      [(list (? impl-exists? impl) args ...)
       (define args* (map munge args))
       (define vars (impl-info impl 'vars))
       (define node (replace-vars (map cons vars args*) (impl-info impl 'fpcore)))
       (if root?
           node
           (push! impl node))]))

  (define root (munge expr #:root? #t))
  (cons (list->vector (reverse instrs)) root))

;; Step 2.
;; Inlines let bindings; let-inlining is generally unsound with
;; rounding properties (the parent context may change),
;; so we only inline those that result in the same operator
;; implementation when converting back from FPCore to LImpl.

(define (inline! root ivec ctx)
  (define global-prop-dict (repr->prop (context-repr ctx)))
  (let loop ([node root]
             [prop-dict global-prop-dict])
    (match node
      [(? number?) node] ; number
      [(? symbol?) node] ; variable
      [(index idx) ; let-bound variable
       ; we check what happens if we inline
       (match-define (cons expr impl) (vector-ref ivec idx))
       (define impl*
         (match expr
           [(list '! props ... (list op _ ...))
            ; rounding context updated parent context
            (define prop-dict*
              (if (not (null? props))
                  (apply dict-set prop-dict props)
                  prop-dict))
            (assert-fpcore-impl op prop-dict* (impl-info impl 'itype))]
           ; rounding context inherited from parent context
           [(list op _ ...) (assert-fpcore-impl op prop-dict (impl-info impl 'itype))]))
       (cond
         [(equal? impl impl*) ; inlining is safe
          (define expr* (loop expr prop-dict))
          (vector-set! ivec idx #f)
          expr*]
         [else ; inlining is not safe
          (define expr* (loop expr global-prop-dict))
          (vector-set! ivec idx expr*)
          node])]
      [(list '! props ... body) ; explicit rounding context
       (define prop-dict* (props->dict props))
       (define body* (loop body prop-dict*))
       (define new-prop-dict
         (for/list ([(k v) (in-dict prop-dict*)]
                    #:unless (and (dict-has-key? prop-dict k) (equal? (dict-ref prop-dict k) v)))
           (cons k v)))
       (if (null? new-prop-dict)
           body*
           `(! ,@(dict->props new-prop-dict) ,body*))]
      [(list op args ...) ; operator application
       (define args* (map (lambda (e) (loop e prop-dict)) args))
       `(,op ,@args*)])))

;; Step 3.
;; Construct the final FPCore expression using remaining let-bindings
;; and the let-free body from the previous step.

(define (reachable-indices ivec expr)
  (define reachable (mutable-set))
  (let loop ([expr expr])
    (match expr
      [(? number?) (void)]
      [(? symbol?) (void)]
      [(index idx)
       (set-add! reachable idx)
       (loop (vector-ref ivec idx))]
      [(list _ args ...) (for-each loop args)]))
  reachable)

(define (remove-indices id->name expr)
  (let loop ([expr expr])
    (match expr
      [(? number?) expr]
      [(? symbol?) expr]
      [(index idx) (hash-ref id->name idx)]
      [(list '! props ... body) `(! ,@props ,(loop body))]
      [(list op args ...) `(,op ,@(map loop args))])))

(define (build-expr expr ivec ctx)
  ; variable generation
  (define vars (list->mutable-seteq (context-vars ctx)))
  (define counter 0)
  (define (gensym)
    (set! counter (add1 counter))
    (match (string->symbol (format "t~a" counter))
      [(? (curry set-member? vars)) (gensym)]
      [x
       (set-add! vars x)
       x]))

  ; need fresh variables for reachable, non-inlined subexpressions
  (define reachable (reachable-indices ivec expr))
  (define id->name (make-hash))
  (for ([expr (in-vector ivec)]
        [idx (in-naturals)])
    (when (and expr (set-member? reachable idx))
      (hash-set! id->name idx (gensym))))

  (for/fold ([body (remove-indices id->name expr)]) ([idx (in-list (sort (hash-keys id->name) >))])
    (define var (hash-ref id->name idx))
    (define val (remove-indices id->name (vector-ref ivec idx)))
    `(let ([,var ,val]) ,body)))

;; Translates from LImpl to an FPCore.
;; The implementation of this procedure is complicated since
;;  (1) every operator implementation requires certain (FPCore) rounding properties
;;  (2) rounding contexts have lexical scoping
(define (prog->fpcore prog ctx)
  ; step 1: convert to an instruction vector where
  ; each expression is evaluated under explicit rounding contexts
  (match-define (cons ivec root) (prog->let-exprs prog))

  ; step 2: inline nodes
  (define body (inline! root ivec ctx))

  ; step 3: construct the actual FPCore expression from
  ; the remaining let-bindings and body
  (build-expr body ivec ctx))
