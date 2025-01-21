#lang racket
(require (only-in xml write-xexpr xexpr?)
         (only-in fpbench
                  fpcore?
                  supported-by-lang?
                  core->c
                  core->fortran
                  core->java
                  core->python
                  core->julia
                  core->matlab
                  core->wls
                  core->tex
                  expr->tex
                  core->js
                  compilers
                  [core-common-subexpr-elim core-cse]
                  *expr-cse-able?*))

(require "../utils/common.rkt"
         "../syntax/read.rkt"
         "../core/programs.rkt"
         "../syntax/types.rkt"
         "../syntax/sugar.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt")

(provide render-menu
         render-warnings
         render-large
         render-comparison
         render-specification
         render-program
         render-bogosity
         render-help
         render-reproduction
         format-percent
         write-html
         program->fpcore
         program->tex
         fpcore->string
         js-tex-include
         doc-url
         core->c
         core->fortran
         core->java
         core->python
         core->julia
         core->matlab
         core->wls
         core->tex
         expr->tex
         core->js)

(define (write-html xexpr out)
  (fprintf out "<!doctype html>\n")
  (write-xexpr xexpr out))

(define (program->fpcore expr ctx #:ident [ident #f])
  (define body (prog->fpcore expr ctx))
  (if ident
      (list 'FPCore ident (context-vars ctx) body)
      (list 'FPCore (context-vars ctx) body)))

(define (fpcore-add-props core props)
  (match core
    [(list 'FPCore name args expr) `(FPCore ,name ,args ,@props ,expr)]
    [(list 'FPCore args expr) `(FPCore ,args ,@props ,expr)]))

(define (at-least-two-ops? expr)
  (match expr
    [(list op args ...) (ormap list? args)]
    [_ #f]))

(define (fpcore->string core)
  (define-values (ident args props expr)
    (match core
      [(list 'FPCore name (list args ...) props ... expr) (values name args props expr)]
      [(list 'FPCore (list args ...) props ... expr) (values #f args props expr)]))
  (define props* ; make sure each property (name, value) gets put on the same line
    (for/list ([(prop name)
                (in-dict (apply dict-set* '() props))]) ; how to make a list of pairs from a list
      (format "~a ~a" prop name)))
  (define top
    (if ident
        (format "FPCore ~a ~a" ident args)
        (format "FPCore ~a" args)))
  (pretty-format `(,top ,@props* ,expr) #:mode 'display))

(define (doc-url page)
  (format "https://herbie.uwplse.org/doc/~a/~a" *herbie-version* page))

(define/contract (render-menu #:path [path "."] name links)
  (->* (string? (listof (cons/c string? string?))) (#:path string?) xexpr?)
  `(header (h1 ,name)
           (img ([src ,(string-append path "/" "logo-car.png")]))
           (nav (ul ,@(for/list ([(text url) (in-dict (filter identity links))])
                        `(li (a ([href ,url]) ,text)))))))

(define/contract (render-warnings warnings)
  (-> (listof (list/c symbol? string? (listof any/c) (or/c string? #f) (listof string?))) xexpr?)
  (if (null? warnings)
      ""
      `(ul ((class "warnings"))
           ,@(for/list ([warning warnings])
               (match-define (list type message args url extra) warning)
               `(li (h2 ,(apply format message args)
                        ,(if url
                             `(a ([href ,url]) " (more)")
                             ""))
                    ,(if (null? extra)
                         ""
                         `(ol ((class "extra"))
                              ,@(for/list ([line extra])
                                  `(li ,line)))))))))

(define (render-large #:title [title #f] name . values)
  `(div ,name
        ": "
        (span ((class "number") ,@(if title
                                      `([title ,title])
                                      '()))
              ,@values)))

(define (render-comparison #:title [title #f] name a b)
  (render-large #:title title name a `(span ((class "unit")) " → ") b))

(define (render-specification test #:bogosity [bogosity #f])
  (define-values (dropdown body)
    (render-program (test-spec test)
                    (test-context test)
                    #:pre (test-pre test)
                    #:ident (test-identifier test)))
  `(section (details ([id "specification"] (class "programs"))
                     (summary (h2 "Specification")
                              ,dropdown
                              (a ((class "help-button float") [href ,(doc-url "report.html#spec")]
                                                              [target "_blank"])
                                 "?"))
                     ,body
                     ,@(cond
                         [bogosity
                          `((p "Sampling outcomes in "
                               (kbd ,(~a (representation-name (test-output-repr test))))
                               " precision:")
                            ,(render-bogosity bogosity))]
                         [else '()]))))

(define languages
  `(("FPCore" "fpcore" ,(λ (c i) (fpcore->string c))) ("C" "c" ,core->c)
                                                      ("Fortran" "f03" ,core->fortran)
                                                      ("Java" "java" ,core->java)
                                                      ("Python" "py" ,core->python)
                                                      ("Julia" "jl" ,core->julia)
                                                      ("MATLAB" "mat" ,core->matlab)
                                                      ("Wolfram" "wl" ,core->wls)
                                                      ("TeX" "tex" ,(λ (c i) (core->tex c)))))

(define (program->tex prog ctx #:loc [loc #f])
  (define prog* (program->fpcore prog ctx))
  (if (supported-by-lang? prog* "tex")
      (core->tex prog* #:loc (and loc (cons 2 loc)) #:color "blue")
      "ERROR"))

(define (combine-fpcore-instruction i e c)
  (match i
    [(list 'abs x)
     (define x* (string->symbol (string-append (symbol->string x) "_m")))
     (define e* (replace-expression e x x*))
     (define p (index-of (context-vars c) x))
     (define c* (struct-copy context c [vars (list-set (context-vars c) p x*)]))
     (cons e* c*)]
    [(list 'negabs x)
     (define x-string (symbol->string x))
     (define x-sign (string->symbol (string-append x-string "_s")))
     (define x* (string->symbol (string-append x-string "_m")))
     (define p (index-of (context-vars c) x))
     (define r (list-ref (context-var-reprs c) p))
     (define c* (struct-copy context c [vars (list-set (context-vars c) p x*)]))
     (define c** (context-extend c* x-sign r))
     (define *-impl (get-fpcore-impl '* (repr->prop (context-repr c)) (list r (context-repr c))))
     (define e* (list *-impl x-sign (replace-expression e x x*)))
     (cons e* c**)]
    [_ (cons e c)]))

(define (format-prelude-instruction instruction ctx ctx* language converter)
  (define (converter* e c)
    (define fpcore (program->fpcore e c))
    (define output (converter fpcore "code"))
    (define lines (string-split output "\n"))
    (match language
      ["FPCore" (pretty-format e #:mode 'display)]
      ["Fortran" (string-trim (third lines) #px"\\s+code\\s+=\\s+")]
      ["MATLAB" (string-trim (second lines) #px"\\s+tmp\\s+=\\s+")]
      ["Wolfram" (string-trim (first lines) #px".*:=\\s+")]
      ["TeX" output]
      [_ (string-trim (second lines) #px"\\s+return\\s+")]))
  (match instruction
    [(list 'abs x)
     (define x* (string->symbol (string-append (symbol->string x) "_m")))
     (define r (list-ref (context-var-reprs ctx) (index-of (context-vars ctx) x)))
     (define fabs-impl (get-fpcore-impl 'fabs (repr->prop r) (list r)))
     (define e (list fabs-impl x))
     (define c (context (list x) r r))
     (list (format "~a = ~a" x* (converter* e c)))]
    [(list 'negabs x)
     ; TODO: why are x* and x-sign unused?
     (define x* (string->symbol (format "~a_m" x)))
     (define r (context-lookup ctx x))
     (define fabs-impl (get-fpcore-impl 'fabs (repr->prop r) (list r)))
     (define copysign-impl (get-fpcore-impl 'copysign (repr->prop r) (list r r)))
     (define e* (list fabs-impl x))
     (define x-sign (string->symbol (format "~a_s" x)))
     (define e-sign (list copysign-impl (literal 1 (representation-name r)) x))
     (define c (context (list x) r r))
     (list (format "~a = ~a" (format "~a\\_m" x) (converter* e* c))
           (format "~a = ~a" (format "~a\\_s" x) (converter* e-sign c)))]
    [(list 'sort vs ...)
     (define vs (context-vars ctx))
     (define vs* (context-vars ctx*))
     ;; We added some sign-* variables to the front of the variable
     ;; list in `ctx*`, we only want the originals here
     (list (format-sort-instruction (take-right vs* (length vs)) language))]))

(define (format-sort-instruction vs l)
  (match l
    ["C" (format "assert(~a);" (format-less-than-condition vs))]
    ["Java" (format "assert ~a;" (format-less-than-condition vs))]
    ["Python"
     (define comma-joined (comma-join vs))
     (format "[~a] = sort([~a])" comma-joined comma-joined)]
    ["Julia"
     (define comma-joined (comma-join vs))
     (format "~a = sort([~a])" comma-joined comma-joined)]
    ["MATLAB"
     (define comma-joined (comma-join vs))
     (format "~a = num2cell(sort([~a])){:}" comma-joined comma-joined)]
    ["TeX"
     (define comma-joined (comma-join vs))
     (format "[~a] = \\mathsf{sort}([~a])\\\\" comma-joined comma-joined)]
    [_
     (match vs
       [(list x y) (format sort-note (format "~a and ~a" x y))]
       [(list vs ...)
        (format sort-note
                (string-join (map ~a vs)
                             ", "
                             ;; "Lil Jon, he always tells the truth"
                             #:before-last ", and "))])]))

(define (format-less-than-condition variables)
  (string-join (for/list ([a (in-list variables)]
                          [b (in-list (cdr variables))])
                 (format "~a < ~a" a b))
               " && "))

(define (comma-join vs)
  (string-join (map ~a vs) ", "))

(define sort-note "NOTE: ~a should be sorted in increasing order before calling this function.")

(define (render-program expr
                        ctx
                        #:ident [identifier #f]
                        #:pre [precondition '(TRUE)]
                        #:instructions [instructions empty])
  (define output-repr (context-repr ctx))
  (match-define (cons expr* ctx*)
    (foldl (match-lambda*
             [(list i (cons e c)) (combine-fpcore-instruction i e c)])
           (cons expr ctx)
           instructions))
  (define out-prog
    (parameterize ([*expr-cse-able?* at-least-two-ops?])
      (core-cse (program->fpcore expr* ctx* #:ident identifier))))

  (define output-prec (representation-name output-repr))
  (define out-prog* (fpcore-add-props out-prog (list ':precision output-prec)))

  (define versions
    (reap [sow]
          (for ([(lang record) (in-dict languages)])
            (match-define (list ext converter) record)
            (when (and (fpcore? out-prog*)
                       (or (equal? ext "fpcore") (supported-by-lang? out-prog* ext)))
              (define name
                (if identifier
                    (symbol->string identifier)
                    "code"))
              (define out (converter out-prog* name))
              (define prelude-lines
                (string-join
                 (append-map (lambda (instruction)
                               (format-prelude-instruction instruction ctx ctx* lang converter))
                             instructions)
                 (if (equal? lang "TeX") "\\\\\n" "\n")
                 #:after-last "\n"))
              (sow (cons lang
                         ((if (equal? lang "TeX")
                              (curry format "\\begin{array}{l}\n~a\\\\\n~a\\end{array}\n")
                              string-append)
                          prelude-lines
                          out)))))))

  (define math-out (dict-ref versions "TeX" ""))

  (define dropdown
    `(select (option "Math")
             ,@(for/list ([lang (in-dict-keys versions)])
                 `(option ,lang))))

  (define body
    `(div
      ,(if (equal? precondition '(TRUE))
           ""
           `(div
             ([id "precondition"])
             (div ((class "program math")) "\\[" ,(expr->tex (prog->fpcore precondition ctx)) "\\]")))
      (div ((class "implementation") [data-language "Math"])
           (div ((class "program math")) "\\[" ,math-out "\\]"))
      ,@(for/list ([(lang out) (in-dict versions)])
          `(div ((class "implementation") [data-language ,lang]) (pre ((class "program")) ,out)))))

  (values dropdown body))

(define/contract (render-command-line)
  (-> string?)
  (format "herbie shell --seed ~a ~a"
          (if (vector? (get-seed))
              (format "'~a'" (get-seed))
              (get-seed))
          (string-join (for/list ([rec (changed-flags)])
                         (match rec
                           [(list 'enabled class flag) (format "+o ~a:~a" class flag)]
                           [(list 'disabled class flag) (format "-o ~a:~a" class flag)]))
                       " ")))

(define/contract (render-fpcore test)
  (-> test? string?)
  (define output-repr (test-output-repr test))
  (string-join
   (filter identity
           (list (if (test-identifier test)
                     (format "(FPCore ~a ~a" (test-identifier test) (test-vars test))
                     (format "(FPCore ~a" (test-vars test)))
                 (format "  :name ~s" (test-name test))
                 (format "  :precision ~s" (representation-name (test-output-repr test)))
                 (if (equal? (test-pre test) '(TRUE))
                     #f
                     (format "  :pre ~a" (prog->fpcore (test-pre test) (test-context test))))
                 (if (equal? (test-expected test) #t)
                     #f
                     (format "  :herbie-expected ~a" (test-expected test)))
                 (and (test-output test)
                      (not (null? (test-output test)))
                      (format "\n~a"
                              (string-join (map (lambda (exp) (format "  :alt\n  ~a\n" (car exp)))
                                                (test-output test))
                                           "\n")))
                 (format "  ~a)" (prog->fpcore (test-input test) (test-context test)))))
   "\n"))

(define (format-percent num den)
  (string-append (if (zero? den)
                     (cond
                       [(positive? num) "+∞"]
                       [(zero? num) "0"]
                       [(negative? num) "-∞"])
                     (~r (* (/ num den) 100) #:precision '(= 1)))
                 "%"))

(define (render-bogosity domain-info)
  (define total (round (apply + (hash-values domain-info))))
  (define tags '(valid unknown infinite unsamplable invalid precondition))
  `(div ((class "bogosity"))
        ,@(for/list ([tag tags])
            (define pct (format-percent (hash-ref domain-info tag 0) total))
            `(div ((class ,(format "bogosity-~a" tag)) [data-id ,(format "bogosity-~a" tag)]
                                                       [data-type ,(~a tag)]
                                                       [data-timespan
                                                        ,(~a (hash-ref domain-info tag 0))]
                                                       [title ,(format "~a (~a)" tag pct)])))))

(define/contract (render-reproduction test #:bug? [bug? #f])
  (->* (test?) (#:bug? boolean?) xexpr?)

  `(section
    ([id "reproduce"])
    (details ,(if bug?
                  '([open "open"])
                  "")
             (summary (h2 "Reproduce")
                      (a ((class "help-button float") [href ,(doc-url "report.html#reproduction")]
                                                      [target "_blank"])
                         "?"))
             (pre ((class "shell")) (code ,(render-command-line) "\n" ,(render-fpcore test) "\n"))
             ,(if bug?
                  `(p "Please file a "
                      (a ((href "https://github.com/herbie-fp/herbie/issues")) "bug report")
                      " with this information.")
                  ""))))

(define (render-help url #:float [float? #t])
  `(a ((class ,(if float? "help-button float" "help-button")) [href ,(doc-url url)] [target "_blank"])
      "?"))

(define js-tex-include
  '((link ([rel "stylesheet"]
           [href "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css"]
           [integrity "sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH"]
           [crossorigin "anonymous"]))
    (script ([src "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js"]
             [integrity "sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm"]
             [crossorigin "anonymous"]))
    (script ([src "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/contrib/auto-render.min.js"]
             [integrity "sha384-aGfk5kvhIq5x1x5YdvCp4upKZYnA8ckafviDpmWEKp4afOZEqOli7gqSnh8I6enH"]
             [crossorigin "anonymous"]))))
