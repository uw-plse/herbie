#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../core/programs.rkt"
         "types.rkt"
         "syntax.rkt"
         "platform.rkt"
         "syntax-check.rkt"
         "type-check.rkt"
         "sugar.rkt"
         "load-plugin.rkt")

(provide (struct-out test)
         test-context
         test-output-repr
         load-tests
         parse-test)

(struct test
        (name identifier
              vars
              input
              output
              expected
              spec
              pre
              preprocess
              output-repr-name
              var-repr-names)
  #:prefab)

(define (test-output-repr test)
  (get-representation (test-output-repr-name test)))

(define (test-context test)
  (define output-repr (get-representation (test-output-repr-name test)))
  (define vars (test-vars test))
  (define var-reprs
    (for/list ([var vars])
      (get-representation (dict-ref (test-var-repr-names test) var))))
  (context (test-vars test) output-repr var-reprs))

;; Unfortunately copied from `src/syntax/sugar.rkt`
(define (expand stx)
  (match stx
    ; expand let statements
    [#`(let* ([#,vars #,vals] ...) #,body)
     (datum->syntax #f
                    (list 'let*
                          (for/list ([var (in-list vars)]
                                     [val (in-list vals)])
                            (list var (expand val)))
                          (expand body))
                    stx)]
    [#`(let ([#,vars #,vals] ...) #,body)
     (datum->syntax #f
                    (list 'let
                          (for/list ([var (in-list vars)]
                                     [val (in-list vals)])
                            (list var (expand val)))
                          (expand body))
                    stx)]
    ; special nullary operators
    [#`(,(or 'and 'or)) (datum->syntax #f 'TRUE stx)]
    [#`(+)
     (warn 'nullary-operator "+ is deprecated as a nullary operator")
     (datum->syntax #f 0 stx)]
    [#`(*)
     (warn 'nullary-operator "* is deprecated as a nullary operator")
     (datum->syntax #f 1 stx)]
    ; special unary operators
    [#`(,(or 'and 'or) #,a) (expand a)]
    ; deprecated unary operators
    [#`(,(and (or '+ '*) op) #,a)
     (warn 'unary-operator "~a is deprecated as a unary operator" op)
     (expand a)]
    [#`(/ #,a)
     (warn 'unary-operator "/ is deprecated as a unary operator")
     (datum->syntax #f (list '/ 1 (expand a)) stx)]
    ; binary operators
    [#`(,(and (or '+ '- '* '/ 'or) op) #,arg1 #,arg2)
     (datum->syntax #f (list op (expand arg1) (expand arg2)) stx)]
    ; variary operators
    [#`(,(and (or '+ '- '* '/ 'or) op) #,arg1 #,arg2 #,rest ...)
     (unless (null? rest)
       (warn 'variary-operator "~a is deprecated as a variary operator" op))
     (define prev (datum->syntax #f (list op (expand arg1) (expand arg2)) stx))
     (let loop ([prev prev]
                [rest rest])
       (match rest
         [(list) prev]
         [(list next rest ...)
          (define prev* (datum->syntax #f (list op prev (expand next)) next))
          (loop prev* rest)]))]
    [#`(,(and (or '< '<= '> '>= '=) op) #,args ...)
     (define args* (map expand args))
     (define out
       (for/fold ([out #f])
                 ([term args*]
                  [next (cdr args*)])
         (datum->syntax #f
                        (if out
                            (list 'and out (list op term next))
                            (list op term next))
                        term)))
     (or out (datum->syntax #f 'TRUE stx))]
    [#`(!= #,args ...)
     (define args* (map expand args))
     (define out
       (for/fold ([out #f])
                 ([term args*]
                  [i (in-naturals)]
                  #:when #t
                  [term2 args*]
                  [j (in-naturals)]
                  #:when (< i j))
         (datum->syntax #f
                        (if out
                            (list 'and out (list '!= term term2))
                            (list '!= term term2))
                        stx)))
     (or out (datum->syntax #f 'TRUE stx))]
    ; other operators
    [#`(#,op #,args ...) (datum->syntax #f (cons op (map expand args)) stx)]
    ; numbers, variables
    [_ stx]))

(define (expand-core stx)
  (match stx
    [#`(FPCore #,name (#,vars ...) #,props ... #,body)
     (datum->syntax #f (append (list 'FPCore name vars) props (list (expand body))) stx)]
    [#`(FPCore (#,vars ...) #,props ... #,body)
     (datum->syntax #f (append (list 'FPCore vars) props (list (expand body))) stx)]))

(define (parse-platform-name ann)
  (match ann
    [(list '! props ...)
     (let loop ([props props])
       (match props
         [(list ':herbie-platform name _ ...) name]
         [(list _ _ rest ...) (loop rest)]
         [(list) #f]))]
    [_ #f]))

(define (parse-test stx)
  (assert-program! stx)
  (define stx* (expand-core stx))
  (assert-program-typed! stx*)
  (define-values (func-name args props body)
    (match (syntax->datum stx*)
      [(list 'FPCore name (list args ...) props ... body) (values name args props body)]
      [(list 'FPCore (list args ...) props ... body) (values #f args props body)]))

  ;; Do not do this for the below code : (define prop-dict (props->dict props))
  ;; Yes, the above is more efficient but it DOES not work
  ;; Why? : props->dict on paper makes sense but :alt is not a unique key. TO prevent
  ;;        there from being only one Developer Target, let this inefficient code exist
  ;;        until we find something that exists that allows duplicate keys and dict-ref
  (define prop-dict
    (let loop ([props props])
      (match props
        ['() '()]
        [(list prop val rest ...) (cons (cons prop val) (loop rest))])))

  (define default-prec (dict-ref prop-dict ':precision (*default-precision*)))

  (define-values (var-names var-precs)
    (for/lists (var-names var-precs)
               ([var (in-list args)])
               (match var
                 [(list '! props ... name)
                  (define prop-dict (props->dict props))
                  (define arg-prec (dict-ref prop-dict ':precision default-prec))
                  (values name arg-prec)]
                 [(? symbol? name) (values name default-prec)])))

  (define default-repr (get-representation default-prec))
  (define var-reprs (map get-representation var-precs))
  (define ctx (context var-names default-repr var-reprs))

  ;; Named fpcores need to be added to function table
  (when func-name
    (register-function! func-name args default-prec body))

  ;; Try props first, then identifier, else the expression itself
  (define name (or (dict-ref prop-dict ':name #f) func-name body))

  ;; inline and desugar
  (define body* (fpcore->prog body ctx))
  (define pre* (fpcore->prog (dict-ref prop-dict ':pre 'TRUE) ctx))

  (define targets
    (begin
      ; (printf "prop-dict ~a\n\n" prop-dict)

      (for/list ([(key val) (in-dict prop-dict)]
                 #:when (eq? key ':alt))

        ; (printf "key ~a\n" key)
        ; (printf "val ~a\n" val)

        (match (parse-platform-name val) ; plat-name is symbol or #f
          ; If plat-name extracted, check if name matches
          [(? symbol? plat-name) (cons val (equal? plat-name (*platform-name*)))]
          ; try to lower
          [#f
           (with-handlers ([exn:fail:user:herbie:missing? (lambda (e) (cons val #f))])
             ; Testing if error thrown
             (fpcore->prog val ctx)
             (cons val #t))]))))

  (define spec (fpcore->prog (dict-ref prop-dict ':spec body) ctx))
  (check-unused-variables var-names body* pre*)
  (check-weird-variables var-names)

  (test (~a name)
        func-name
        var-names
        body*
        targets
        (dict-ref prop-dict ':herbie-expected #t)
        spec
        pre*
        (dict-ref prop-dict ':herbie-preprocess empty)
        (representation-name default-repr)
        (for/list ([var (in-list var-names)]
                   [repr (in-list var-reprs)])
          (cons var (representation-name repr)))))

(define (check-unused-variables vars precondition expr)
  ;; Fun story: you might want variables in the precondition that
  ;; don't appear in the `expr`, because that can allow you to do
  ;; non-uniform sampling. For example, if you have the precondition
  ;; `(< x y)`, where `y` is otherwise unused, then `x` is sampled
  ;; non-uniformly (biased toward small values).
  (define used (set-union (free-variables expr) (free-variables precondition)))
  (unless (set=? vars used)
    (define unused (set-subtract vars used))
    (warn 'unused-variable
          #:url "faq.html#unused-variable"
          "unused ~a ~a"
          (if (equal? (set-count unused) 1) "variable" "variables")
          (string-join (map ~a unused) ", "))))

(define (check-weird-variables vars)
  (for* ([var vars]
         [const (all-constants)])
    (when (string-ci=? (symbol->string var) (symbol->string const))
      (warn 'strange-variable
            #:url "faq.html#strange-variable"
            "unusual variable ~a; did you mean ~a?"
            var
            const))))

(define (our-read-syntax port name)
  (parameterize ([read-decimal-as-inexact false])
    (read-syntax port name)))

(define (load-stdin)
  (for/list ([test (in-port (curry our-read-syntax "stdin") (current-input-port))])
    (parse-test test)))

(define (load-file file)
  (call-with-input-file file
                        (λ (port)
                          (port-count-lines! port)
                          (for/list ([test (in-port (curry our-read-syntax file) port)])
                            (parse-test test)))))

(define (load-directory dir)
  (apply append
         (for/list ([fname (in-directory dir)]
                    #:when (file-exists? fname)
                    #:when (equal? (filename-extension fname) #"fpcore"))
           (load-file fname))))

(define (load-tests path)
  (define path*
    (if (string? path)
        (string->path path)
        path))
  (define out
    (cond
      [(equal? path "-") (load-stdin)]
      [(directory-exists? path*) (load-directory path*)]
      [else (load-file path*)]))
  (define duplicates (find-duplicates (map test-name out)))
  (unless (null? duplicates)
    (warn 'duplicate-names
          "Duplicate ~a ~a used for multiple cores"
          (if (equal? (length duplicates) 1) "name" "names")
          (string-join (map (curry format "\"~a\"") duplicates) ", ")))
  out)

(module+ test
  (require rackunit
           "load-plugin.rkt")
  (load-herbie-builtins)

  (define precision 'binary64)
  (define ctx (make-debug-context '(x y z a)))

  ;; inlining

  ;; Test classic quadp and quadm examples
  (register-function! 'discr (list 'a 'b 'c) precision `(sqrt (- (* b b) (* a c))))
  (define quadp `(/ (+ (- y) (discr x y z)) x))
  (define quadm `(/ (- (- y) (discr x y z)) x))
  (check-equal? (fpcore->prog quadp ctx)
                '(/.f64 (+.f64 (neg.f64 y) (sqrt.f64 (-.f64 (*.f64 y y) (*.f64 x z)))) x))
  (check-equal? (fpcore->prog quadm ctx)
                '(/.f64 (-.f64 (neg.f64 y) (sqrt.f64 (-.f64 (*.f64 y y) (*.f64 x z)))) x))

  ;; x^5 = x^3 * x^2
  (register-function! 'sqr (list 'x) precision '(* x x))
  (register-function! 'cube (list 'x) precision '(* x x x))
  (define fifth '(* (cube a) (sqr a)))
  (check-equal? (fpcore->prog fifth ctx) '(*.f64 (*.f64 (*.f64 a a) a) (*.f64 a a)))

  ;; casting edge cases
  (check-equal? (fpcore->prog `(cast x) ctx) 'x)
  (check-equal? (fpcore->prog `(cast (! :precision binary64 x)) ctx) 'x))
