#lang racket

(require racket/file
         "programs.rkt"
         "rules.rkt"
         "../syntax/matcher.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../config.rkt"
         "../syntax/load-plugin.rkt"
         "../utils/timeline.rkt"
         "batch.rkt"
         "egg-herbie.rkt"
         "egglog-program.rkt")

(provide prelude
         prelude-exprs
         egglog-add-exprs
         egglog-add-exprs-mainloop
         make-egglog-runner
         run-egglog-multi-extractor
         run-egglog-proofs
         run-egglog-equal?
         e2->expr
         e1->expr
         populate-e->id-tables
         egglog-var?)

(module+ test
  (require rackunit)
  (require "../syntax/load-plugin.rkt")
  (load-herbie-builtins))

(define op-string-names
  (hash '+ 'Add '- 'Sub '* 'Mul '/ 'Div '== 'Eq '!= 'Neq '> 'Gt '< 'Lt '>= 'Gte '<= 'Lte))

(define/reset id->e1 (make-hasheq))
(define/reset e1->id (make-hasheq))
(define/reset id->e2 (make-hasheq))
(define/reset e2->id (make-hasheq))

;; [Copied from egg-herbie.rkt] Returns all representatations (and their types) in the current platform.
(define (all-repr-names [pform (*active-platform*)])
  (remove-duplicates (map (lambda (repr) (representation-name repr)) (platform-reprs pform))))

; Types handled
; - rationals
; - string

;; High-level function that writes the program to a file, runs it then returns output
(define (process-egglog program)
  (define curr-program (get-actual-program program))

  (define egglog-file-path
    (let ([temp-file (make-temporary-file "program-to-egglog-~a.egg")])
      (with-output-to-file temp-file #:exists 'replace (lambda () (for-each writeln curr-program)))
      temp-file))

  (define egglog-path
    (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

  (define stdout-port (open-output-string))
  (define stderr-port (open-output-string))

  (define old-error-port (current-error-port))

  ;; Run egglog and capture output
  (parameterize ([current-output-port stdout-port]
                 [current-error-port stderr-port])
    (unless (system (format "~a ~a" egglog-path egglog-file-path))
      (begin
        (fprintf old-error-port "stdout-port ~a\n" (get-output-string stdout-port))
        ; Tail the last 100 lines of the error instead of everything
        (fprintf old-error-port
                 "stderr-port ~a\n"
                 (string-join (take-right (string-split (get-output-string stderr-port) "\n") 100)
                              "\n"))
        (fprintf old-error-port "incorrect program ~a\n" curr-program)
        (error "Failed to execute egglog"))))

  (delete-file egglog-file-path)

  (cons (get-output-string stdout-port) (get-output-string stderr-port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;; Most calls to egglog should be done through this interface.
;;  - `make-egglog-runner`: creates a struct that describes a _reproducible_ egglog instance
;;  - `run-egglog`: takes an egglog runner and performs an extraction (exprs or proof)

;; Herbie's version of an egglog runner.
;; Defines parameters for running rewrite rules with egglog
(struct egglog-runner (batch roots reprs schedule ctx)
  #:transparent ; for equality
  #:methods gen:custom-write ; for abbreviated printing
  [(define (write-proc alt port mode)
     (fprintf port "#<egglog-runner>"))])

;; Constructs an egglog runner. Exactly same as egg-runner
;; But needs some amount of specifics - TODO
;;
;; The schedule is a list of pairs specifying
;;  - a list of rules
;;  - scheduling parameters:
;;     - node limit: `(node . <number>)`
;;     - iteration limit: `(iteration . <number>)`
;;     - constant fold: `(const-fold? . <boolean>)` [default: #t]
;;     - scheduler: `(scheduler . <name>)` [default: backoff]
;;        - `simple`: run all rules without banning
;;        - `backoff`: ban rules if the fire too much
(define (make-egglog-runner batch roots reprs schedule #:context [ctx (*context*)])
  (define (oops! fmt . args)
    (apply error 'verify-schedule! fmt args))
  ; verify the schedule
  (for ([instr (in-list schedule)])
    (match instr
      [(cons rules params)
       ;; `run` instruction
       (unless (and (list? rules) (andmap rule? rules))
         (oops! "expected list of rules: `~a`" rules))
       (for ([param (in-list params)])
         (match param
           [(cons 'node (? nonnegative-integer?)) (void)]
           [(cons 'iteration (? nonnegative-integer?)) (void)]
           [(cons 'const-fold? (? boolean?)) (void)]
           [(cons 'scheduler mode)
            (unless (set-member? '(simple backoff) mode)
              (oops! "in instruction `~a`, unknown scheduler `~a`" instr mode))]
           [_ (oops! "in instruction `~a`, unknown parameter `~a`" instr param)]))]
      [_ (oops! "expected `(<rules> . <params>)`, got `~a`" instr)]))
  ; make the runner
  (egglog-runner batch roots reprs schedule ctx))

;; 2. 4 types of run-egglog
;; Runs egg using an egg runner.
;;
;; Argument `cmd` specifies what to get from the e-graph:
;;  - single extraction: `(single . <extractor>)`
;;  - multi extraction: `(multi . <extractor>)`
;;  - proofs: `(proofs . ((<start> . <end>) ...))`

;; TODO : Need to run egglog to get the actual ids
;; very hard - per id recruse one level and ger simplest child
(define (run-egglog-multi-extractor runner
                                    extractor
                                    #:num-variants [num-variants #t]) ; multi expression extraction

  (define curr-batch (batch-remove-zombie (egg-runner-batch runner) (egg-runner-roots runner)))

  (define curr-program (make-egglog-program))

  ;; 1. Add the Prelude
  (prelude curr-program #:mixed-egraph? #t)

  ;; 2. User Rules which comes from schedule (need to be translated)
  (define tag-schedule
    (for/list ([i (in-naturals 1)]
               [element (in-list (egg-runner-schedule runner))])

      (define rule-type (car element))
      (define schedule-params (cdr element))
      (define tag
        (match rule-type
          ['lift 'lifting]
          ['lower 'lowering]
          [_
           (define curr-tag (string->symbol (string-append "?tag" (number->string i))))
           ;; Add rulesets
           (egglog-program-add! `(ruleset ,curr-tag) curr-program)

           ;; Add the actual egglog rewrite rules
           (egglog-program-add-list! (egglog-rewrite-rules rule-type curr-tag) curr-program)

           curr-tag]))

      (cons tag schedule-params)))

  ;; 3. Inserting expressions -> (egglog-add-exprs curr-batch (egglog-runner-ctx))
  ; (exprs . extract bindings)
  ; (define egglog-batch-exprs (prev-egglog-add-exprs curr-batch (egg-runner-ctx runner)))

  (define extract-bindings (egglog-add-exprs curr-batch (egg-runner-ctx runner) curr-program))

  ; (set! program (append program (car egglog-batch-exprs)))
  ; (egglog-program-add-list! (car egglog-batch-exprs) curr-program)

  ;; 4. Running the schedule
  (define run-schedule '())
  (define domain-fns '())
  (for ([(tag schedule-params) (in-dict tag-schedule)])
    (match tag
      ['lifting
       (set! domain-fns (cons tag domain-fns))
       (set! run-schedule (append run-schedule (list (list 'saturate tag))))]
      ['lowering
       (set! domain-fns (cons tag domain-fns))
       (set! run-schedule
             (append run-schedule (list (list 'repeat 2 'const-fold) (list 'saturate tag))))]
      [_
       ; Set params
       (define is-node-present (dict-ref schedule-params 'node #f))
       (define is-iteration-present (dict-ref schedule-params 'iteration #f))

       (match* (is-node-present is-iteration-present)
         [((? nonnegative-integer? node-amt) (? nonnegative-integer? iter-amt))
          (set! run-schedule (append run-schedule `((repeat ,iter-amt ,tag))))]

         [(#f (? nonnegative-integer? iter-amt))
          (set! run-schedule (append run-schedule `((repeat ,iter-amt ,tag))))]

         [((? nonnegative-integer? node-amt) #f)
          (set! run-schedule (append run-schedule `((repeat 3 ,tag))))]

         [(#f #f) `((repeat 3 ,tag))])]))

  ; (set! program (append program `((run-schedule ,@run-schedule))))
  (egglog-program-add! `(run-schedule ,@run-schedule) curr-program)

  ;; 5. Extraction -> should just need root ids
  (for ([binding extract-bindings])
    (define val
      (if num-variants
          `(extract ,binding 5)

          (match domain-fns
            [(list 'lifting) `(extract (lift ,binding))]
            [(list 'lowering)
             (define curr-val
               (symbol->string (representation-name (context-repr (egg-runner-ctx runner)))))
             `(extract (lower ,binding ,curr-val))]
            [_ `(extract ,binding)])))

    ; (set! program (append program val))
    (egglog-program-add! val curr-program))

  ;; 6. After step-by-step building the program, process it
  ;; by running it using egglog
  (define egglog-output (process-egglog curr-program))

  ;; Extract its returned value
  (define stdout-content (car egglog-output))
  ; (define stderr-content (cdr egglog-output))

  (define input-batch (egg-runner-batch runner))
  (define out (batch->mutable-batch input-batch))

  ;; (Listof (Listof exprs))
  (define herbie-exprss
    (let ([input-port (open-input-string stdout-content)])
      (for/list ([next-expr (in-port read input-port)])
        (if num-variants
            (map e2->expr next-expr)
            (list (e2->expr next-expr))))))

  (define result
    (for/list ([variants (in-list herbie-exprss)])
      (remove-duplicates
       (for/list ([v (in-list variants)])
         (egglog->batchref v input-batch out (context-repr (egg-runner-ctx runner))))
       #:key batchref-idx)))

  (batch-copy-mutable-nodes! input-batch out)

  ;; (Listof (Listof batchref))
  result)

(define (egglog->batchref expr input-batch out type)
  (define idx
    (let loop ([expr expr]
               [type type])
      (define term
        (match expr
          [(? number?)
           (if (representation? type)
               (literal expr (representation-name type))
               expr)]
          [(? symbol?) expr]
          [`(if ,cond ,ift ,iff)
           (if (representation? type)
               (list 'if (loop cond (get-representation 'bool)) (loop ift type) (loop iff type))
               (list 'if (loop cond 'bool) (loop ift type) (loop iff type)))]
          [(approx spec impl) (approx (loop spec #f) (loop impl type))]
          [(list (? impl-exists? impl) args ...) (cons impl (map loop args (impl-info impl 'itype)))]
          [(list op args ...)
           (define args*
             (for/list ([arg (in-list args)])
               (loop arg #f)))
           (cons op args*)]))
      (mutable-batch-push! out term)))
  (batchref input-batch idx))

;; egglog does not have proof
;; there is some value that herbie has which indicates we could not
;; find a proof. Might be (list #f #f ....)
(define (run-egglog-proofs runner rws) ; proof extraction
  (for/list ([(start-expr end-expr) (in-dict rws)])
    #f))

; ; 1. ask within egglog program what is id
; ; 2. Extract expression from each expr
(define (run-egglog-equal? runner expr-pairs) ; term equality?
  (define curr-program (make-egglog-program))

  ;; 1. Add the Prelude
  (prelude curr-program #:mixed-egraph? #t)

  ;; 2. User Rules which comes from schedule (need to be translated)
  (for ([i (in-naturals 1)]
        [element (in-list (egg-runner-schedule runner))])

    (define rule-type (car element))
    (define schedule-params (cdr element))

    ;; Create a custom tag
    (define tag (string->symbol (string-append "?tag" (number->string i))))

    ;; Add rulesets
    (egglog-program-add! `(ruleset ,tag) curr-program)

    ;; Add the actual egglog rewrite rules
    (egglog-program-add-list! (egglog-rewrite-rules rule-type tag) curr-program))

  ;; 2. Adding each pair of start-expr and end-expr
  (for ([(start-expr end-expr) (in-dict expr-pairs)]
        [i (in-range 1 (length expr-pairs))])

    (define start-let
      `(let ,(string->symbol (string-append "?e1" (number->string i))) ,(expr->e1-expr start-expr)))

    (egglog-program-add! start-let curr-program)

    (define end-let
      `(let ,(string->symbol (string-append "?e2" (number->string i))) ,(expr->e1-expr end-expr)))

    (egglog-program-add! end-let curr-program))

  ;; 4. Running the schedule
  (define run-schedule `(run-schedule (repeat 3 ?tag1) (repeat 20 const-fold)))
  (egglog-program-add! run-schedule curr-program)

  ;; 5. Running Checks
  (for ([i (in-range 1 (length expr-pairs))])
    (define start-extract `(extract ,(string->symbol (string-append "?e1" (number->string i)))))
    (egglog-program-add! start-extract curr-program)

    (define end-extract `(extract ,(string->symbol (string-append "?e2" (number->string i)))))
    (egglog-program-add! end-extract curr-program))

  ;; 6. After step-by-step building the program, process it
  ;; by running it using egglog
  (define egglog-output (process-egglog curr-program))
  (define stdout-content (car egglog-output))
  ; (define stderr-content (cdr egglog-output))

  ;; Extract its returned value
  (define extract-results (list->vector (string-split stdout-content "\n")))

  (for/list ([i (in-range 0 (vector-length extract-results) 2)])
    (equal? (vector-ref extract-results i) (vector-ref extract-results (+ i 1)))))

(define (prelude curr-program #:mixed-egraph? [mixed-egraph? #t])
  (load-herbie-builtins)
  (define pform (*active-platform*))

  (define spec-egraph
    `(datatype M
               (Num BigRat :cost 4294967295)
               (Var String :cost 4294967295)
               (If M M M :cost 4294967295)
               ,@(platform-spec-nodes)))

  (egglog-program-add! spec-egraph curr-program)

  (define typed-graph
    `(datatype MTy
               ,@(num-typed-nodes pform)
               ,@(var-typed-nodes pform)
               (IfTy MTy
                     MTy
                     MTy
                     :cost
                     ,(match (platform-impl-cost pform 'if)
                        [`(max ,n) n] ; Not quite right (copied from egg-herbie.rkt)
                        [`(sum ,n) n]))
               (Approx M MTy)
               ,@(platform-impl-nodes pform)))

  (egglog-program-add! typed-graph curr-program)

  (egglog-program-add! `(constructor lower (M String) MTy :unextractable) curr-program)

  (egglog-program-add! `(constructor lift (MTy) M :unextractable) curr-program)

  (egglog-program-add! `(ruleset const-fold) curr-program)

  (egglog-program-add! `(ruleset lowering) curr-program)

  (egglog-program-add! `(ruleset lifting) curr-program)

  (for ([curr-expr const-fold])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (impl-lowering-rules pform)])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (impl-lifting-rules pform)])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (num-lowering-rules)])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (num-lifting-rules)])
    (egglog-program-add! curr-expr curr-program))

  (for ([curr-expr (if-lowering-rules)])
    (egglog-program-add! curr-expr curr-program))

  (egglog-program-add! (if-lifting-rule) curr-program)

  (egglog-program-add! (approx-lifting-rule) curr-program)

  (void))

(define (prelude-exprs #:mixed-egraph? [mixed-egraph? #t])
  ; (define curr-program (new egglog-program%))
  (define curr-program (make-egglog-program))

  (prelude curr-program #:mixed-egraph? #t)

  (get-actual-program curr-program))

(define const-fold
  `((let ?zero (bigrat
                [from-string "0"]
                [from-string "1"])
      )
    (rewrite (Add (Num x) (Num y)) (Num (+ x y)) :ruleset const-fold)
    (rewrite (Sub (Num x) (Num y)) (Num (- x y)) :ruleset const-fold)
    (rewrite (Mul (Num x) (Num y)) (Num (* x y)) :ruleset const-fold)
    ;(rule ((= e (Div (Num x) (Num y))) (!= ?zero y)) ((union e (Num (/ x y)))) :ruleset const-fold)
    (rewrite (Neg (Num x)) (Num (neg x)) :ruleset const-fold)
    (rule ((= e (Pow (Num x) (Num y))) (= ?zero x) (> y ?zero))
          ((union e (Num ?zero)))
          :ruleset
          const-fold)
    (rule ((= e (Pow (Num x) (Num y))) (= ?zero y) (!= ?zero x))
          ((union e (Num (bigrat (from-string "1") (from-string "1")))))
          :ruleset
          const-fold)
    (rule ((= e (Pow (Num x) (Num y))) (> y ?zero) (!= ?zero x) (= y (round y)))
          ((union e (Num (pow x y))))
          :ruleset
          const-fold)
    (rule ((= e (Log (Num x))) (= (numer x) (denom x))) ((union e (Num ?zero))) :ruleset const-fold)
    (rule ((= e (Cbrt (Num x))) (= (numer x) (denom x)))
          ((union e (Num (bigrat (from-string "1") (from-string "1")))))
          :ruleset
          const-fold)
    (rewrite (Fabs (Num x)) (Num (abs x)) :ruleset const-fold)
    (rewrite (Floor (Num x)) (Num (floor x)) :ruleset const-fold)
    (rewrite (Ceil (Num x)) (Num (ceil x)) :ruleset const-fold)
    (rewrite (Round (Num x)) (Num (round x)) :ruleset const-fold)))

(define (platform-spec-nodes)
  (for/list ([op (in-list (all-operators))])
    (hash-set! (id->e1) op (serialize-op op))
    (hash-set! (e1->id) (serialize-op op) op)
    (define arity (length (operator-info op 'itype)))
    `(,(serialize-op op) ,@(for/list ([i (in-range arity)])
                             'M)
                         :cost
                         4294967295)))

(define (platform-impl-nodes pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define arity (length (impl-info impl 'itype)))
    (define typed-name (string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty")))
    (hash-set! (id->e2) impl typed-name)
    (hash-set! (e2->id) typed-name impl)
    `(,typed-name ,@(for/list ([i (in-range arity)])
                      'MTy)
                  :cost
                  ,(platform-impl-cost pform impl))))

(define (typed-num-id repr-name)
  (string->symbol (string-append "Num" (symbol->string repr-name))))

(define (typed-var-id repr-name)
  (string->symbol (string-append "Var" (symbol->string repr-name))))

(define (num-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(,(typed-num-id repr) BigRat :cost ,(platform-repr-cost pform (get-representation repr)))))

(define (var-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))])
    `(,(typed-var-id repr) String :cost ,(platform-repr-cost pform (get-representation repr)))))

(define (num-lowering-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (Num n)))
           ((let tx ,(symbol->string repr)
              )
            (let etx (,(typed-num-id repr)
                      n)
              )
            (union (lower e tx) etx))
           :ruleset
           lowering)))

(define (num-lifting-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (,(typed-num-id repr) n)))
           ((let se (Num
                     n)
              )
            (union (lift e) se))
           :ruleset
           lifting)))

(define (if-lowering-rules)
  (for/list ([repr (in-list (all-repr-names))])
    `(rule ((= e (If ifc ift iff)) (= tifc (lower ifc "bool"))
                                   (= tift (lower ift ,(symbol->string repr)))
                                   (= tiff (lower iff ,(symbol->string repr))))
           ((let t0 ,(symbol->string repr)
              )
            (let et0 (IfTy
                      tifc
                      tift
                      tiff)
              )
            (union (lower e t0) et0))
           :ruleset
           lowering)))

(define (if-lifting-rule)
  `(rule ((= e (IfTy ifc ift iff)) (= sifc (lift ifc)) (= sift (lift ift)) (= siff (lift iff)))
         ((let se (If
                   sifc
                   sift
                   siff)
            )
          (union (lift e) se))
         :ruleset
         lifting))

(define (approx-lifting-rule)
  `(rule ((= e (Approx spec impl))) ((union (lift e) spec)) :ruleset lifting))

(define (impl-lowering-rules pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define spec-expr (impl-info impl 'spec))
    (define arity (length (impl-info impl 'itype)))
    `(rule ((= e ,(expr->egglog-spec-serialized spec-expr ""))
            ,@(for/list ([v (in-list (impl-info impl 'vars))]
                         [vt (in-list (impl-info impl 'itype))])
                `(= ,(string->symbol (string-append "t" (symbol->string v)))
                    (lower ,v ,(symbol->string (representation-name vt))))))
           ((let t0 ,(symbol->string (representation-name (impl-info impl 'otype)))
              )
            (let et0 (,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
                      ,@(for/list ([v (in-list (impl-info impl 'vars))])
                          (string->symbol (string-append "t" (symbol->string v)))))
              )
            (union (lower e t0) et0))
           :ruleset
           lowering)))

(define (impl-lifting-rules pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define spec-expr (impl-info impl 'spec))
    (define op (string->symbol (car (string-split (symbol->string impl) "."))))
    (define arity (length (impl-info impl 'itype)))
    `(rule ((= e
               (,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
                ,@(impl-info impl 'vars)))
            ,@(for/list ([v (in-list (impl-info impl 'vars))]
                         [vt (in-list (impl-info impl 'itype))])
                `(= ,(string->symbol (string-append "s" (symbol->string v))) (lift ,v))))
           ((let se ,(expr->egglog-spec-serialized spec-expr "s")
              )
            (union (lift e) se))
           :ruleset
           lifting)))

(define (expr->egglog-spec-serialized expr s)
  (let loop ([expr expr])
    (match expr
      [(? number?)
       `(Num (bigrat (from-string ,(number->string (numerator expr)))
                     (from-string ,(number->string (denominator expr)))))]
      [(? symbol?) (string->symbol (string-append s (symbol->string expr)))]
      [(list op args ...)
       `(,(hash-ref (if (hash-has-key? (id->e1) op)
                        (id->e1)
                        (id->e2))
                    op)
         ,@(map loop args))])))

(define (serialize-op op)
  (if (hash-has-key? op-string-names op)
      (hash-ref op-string-names op)
      (string->symbol (string-titlecase (symbol->string op)))))

(define (serialize-impl impl)
  (define impl-split (string-split (symbol->string impl) "."))
  (define op (string->symbol (car impl-split)))
  (define type
    (if (= 2 (length impl-split))
        (cadr impl-split)
        ""))
  (string->symbol (string-append (symbol->string (serialize-op op)) type)))

(define (expr->e2-pattern expr repr)
  (let loop ([expr expr]
             [repr repr])
    (match expr
      [(? literal?)
       `(,(typed-num-id (representation-name repr))
         (bigrat (from-string ,(number->string (numerator (literal-value expr))))
                 (from-string ,(number->string (denominator (literal-value expr))))))]
      [(? symbol?) expr]
      [`(if ,cond ,ift ,iff) `(IfTy ,(loop cond repr) ,(loop ift repr) ,(loop iff repr))]
      [(list op args ...)
       `(,(hash-ref (id->e2) op) ,@(for/list ([arg (in-list args)]
                                              [itype (in-list (impl-info op 'itype))])
                                     (loop arg itype)))])))

(define (expr->e1-pattern expr)
  (let loop ([expr expr])
    (match expr
      [(? number?)
       `(Num (bigrat (from-string ,(number->string (numerator expr)))
                     (from-string ,(number->string (denominator expr)))))]
      [(? symbol?) expr]
      [`(if ,cond ,ift ,iff) `(If ,(loop cond) ,(loop ift) ,(loop iff))]
      [(list op args ...) `(,(hash-ref (id->e1) op) ,@(map loop args))])))

(define (expr->e1-expr expr)
  (let loop ([expr expr])
    (match expr
      [(? number?)
       `(Num (bigrat (from-string ,(number->string (numerator expr)))
                     (from-string ,(number->string (denominator expr)))))]
      [(? symbol?) `(Var ,(symbol->string expr))]
      [`(if ,cond ,ift ,iff) `(If ,(loop cond) ,(loop ift) ,(loop iff))]
      [(list op args ...) `(,(hash-ref (id->e1) op) ,@(map loop args))])))

(define (egglog-rewrite-rules rules tag)
  (for/list ([rule (in-list rules)]
             #:when (not (symbol? (rule-input rule))))
    (if (not (representation? (rule-otype rule)))
        `(rewrite ,(expr->e1-pattern (rule-input rule))
                  ,(expr->e1-pattern (rule-output rule))
                  :ruleset
                  ,tag)
        `(rewrite ,(expr->e2-pattern (rule-input rule) (rule-otype rule))
                  ,(expr->e2-pattern (rule-output rule) (rule-otype rule))
                  :ruleset
                  ,tag))))

(define (egglog-add-exprs-mainloop batch ctx)
  (define curr-program (make-egglog-program))
  (define extract-bindings (egglog-add-exprs batch ctx curr-program))

  (cons (get-actual-program curr-program) extract-bindings))

(define (egglog-add-exprs batch ctx curr-program)
  (define mappings (build-vector (batch-length batch) values))
  (define bindings (make-hash))
  (define vars (make-hash))
  (define (remap x spec?)
    (cond
      [(hash-has-key? vars x)
       (if spec?
           (string->symbol (format "?~a" (hash-ref vars x)))
           (string->symbol (format "?t~a" (hash-ref vars x))))]
      [else (vector-ref mappings x)]))

  ; node -> egglog node binding
  ; inserts an expression into the e-graph, returning binding variable.
  (define (insert-node! node n root?)
    (define binding
      (if root?
          (string->symbol (format "?r~a" n))
          (string->symbol (format "?b~a" n))))
    (hash-set! bindings binding node)
    binding)

  (define root-bindings '())
  ; Inserting nodes bottom-up
  (define root-mask (make-vector (batch-length batch) #f))
  (define spec-mask (make-vector (batch-length batch) #f))
  (define (spec-tag n spec?)
    (when (not (vector-ref spec-mask n))
      (match (vector-ref (batch-nodes batch) n)
        [`(if ,cond ,ift ,iff)
         (when (vector-ref spec-mask cond)
           (vector-set! spec-mask n #t)
           (spec-tag cond #t)
           (spec-tag ift #t)
           (spec-tag iff #t))]
        [(approx spec impl)
         (vector-set! spec-mask n #t)
         (spec-tag spec #t)]
        [(list impl args ...)
         (when (hash-has-key? (id->e1) impl)
           (vector-set! spec-mask n #t)
           (for ([arg (in-list args)])
             (spec-tag arg #t)))]
        [_
         (when spec?
           (vector-set! spec-mask n #t))])))

  (for ([n (in-range (batch-length batch))])
    (spec-tag n #f))
  (for ([root (in-vector (batch-roots batch))])
    (vector-set! root-mask root #t))
  (for ([node (in-vector (batch-nodes batch))]
        [root? (in-vector root-mask)]
        [spec? (in-vector spec-mask)]
        [n (in-naturals)])
    (define node*
      (match node
        [(literal v repr)
         `(,(typed-num-id repr) (bigrat (from-string ,(number->string (numerator v)))
                                        (from-string ,(number->string (denominator v)))))]
        [(? number?)
         `(Num (bigrat (from-string ,(number->string (numerator node)))
                       (from-string ,(number->string (denominator node)))))]
        [(? symbol?) #f]
        [`(if ,cond ,ift ,iff)
         `(,(if spec? 'If 'IfTy) ,(remap cond spec?) ,(remap ift spec?) ,(remap iff spec?))]
        [(approx spec impl) `(Approx ,(remap spec #t) ,(remap impl #f))]
        [(list impl args ...)
         `(,(hash-ref (if spec?
                          (id->e1)
                          (id->e2))
                      impl)
           ,@(for/list ([arg (in-list args)])
               (remap arg spec?)))]))

    (if node*
        (vector-set! mappings n (insert-node! node* n root?))
        (hash-set! vars n node))
    (when root?
      (set! root-bindings (cons (vector-ref mappings n) root-bindings))))

  ; Var-lowering-rules
  (for ([var (in-list (context-vars ctx))]
        [repr (in-list (context-var-reprs ctx))])

    (define curr-var-lowering-rule
      `(rule ((= e (Var ,(symbol->string var))))
             ((let ty ,(symbol->string (representation-name repr))
                )
              (let ety (,(typed-var-id (representation-name repr))
                        ,(symbol->string var))
                )
              (union (lower e ty) ety))
             :ruleset
             lowering))

    (egglog-program-add! curr-var-lowering-rule curr-program))

  ; Var-lifting-rules
  (for ([var (in-list (context-vars ctx))]
        [repr (in-list (context-var-reprs ctx))])

    (define curr-var-lifting-rule
      `(rule ((= e (,(typed-var-id (representation-name repr)) ,(symbol->string var))))
             ((let se (Var
                       ,(symbol->string var))
                )
              (union (lift e) se))
             :ruleset
             lifting))

    (egglog-program-add! curr-var-lifting-rule curr-program))

  ; Var-spec-bindings
  (for ([var (in-list (context-vars ctx))])
    (define curr-var-spec-binding
      `(let ,(string->symbol (format "?~a" var)) (Var ,(symbol->string var))))

    (egglog-program-add! curr-var-spec-binding curr-program))

  ; Var-typed-bindings
  (for ([var (in-list (context-vars ctx))]
        [repr (in-list (context-var-reprs ctx))])
    (define curr-var-typed-binding
      `(let ,(string->symbol (format "?t~a" var))
         (,(typed-var-id (representation-name repr)) ,(symbol->string var))))

    (egglog-program-add! curr-var-typed-binding curr-program))

  ; Binding Exprs
  (for ([root? (in-vector root-mask)]
        [n (in-naturals)]
        #:when (not (hash-has-key? vars n)))
    (define binding
      (if root?
          (string->symbol (format "?r~a" n))
          (string->symbol (format "?b~a" n))))

    (define curr-binding-exprs `(let ,binding ,(hash-ref bindings binding)))

    (egglog-program-add! curr-binding-exprs curr-program))

  ; Only thing returned -> Extract Bindings
  (define extract-bindings
    (for/list ([root (batch-roots batch)])
      (if (hash-has-key? vars root)
          (if (vector-ref spec-mask root)
              (string->symbol (format "?~a" (hash-ref vars root)))
              (string->symbol (format "?t~a" (hash-ref vars root))))
          (string->symbol (format "?r~a" root)))))

  extract-bindings)

(define (prev-egglog-add-exprs batch ctx)
  (define egglog-exprs '())
  (define mappings (build-vector (batch-length batch) values))
  (define bindings (make-hash))
  (define vars (make-hash))
  (define (remap x spec?)
    (cond
      [(hash-has-key? vars x)
       (if spec?
           (string->symbol (format "?~a" (hash-ref vars x)))
           (string->symbol (format "?t~a" (hash-ref vars x))))]
      [else (vector-ref mappings x)]))

  ; node -> egglog node binding
  ; inserts an expression into the e-graph, returning binding variable.
  (define (insert-node! node n root?)
    (define binding
      (if root?
          (string->symbol (format "?r~a" n))
          (string->symbol (format "?b~a" n))))
    (hash-set! bindings binding node)
    binding)

  (define root-bindings '())
  ; Inserting nodes bottom-up
  (define root-mask (make-vector (batch-length batch) #f))
  (define spec-mask (make-vector (batch-length batch) #f))
  (define (spec-tag n spec?)
    (when (not (vector-ref spec-mask n))
      (match (vector-ref (batch-nodes batch) n)
        [`(if ,cond ,ift ,iff)
         (when (vector-ref spec-mask cond)
           (vector-set! spec-mask n #t)
           (spec-tag cond #t)
           (spec-tag ift #t)
           (spec-tag iff #t))]
        [(approx spec impl)
         (vector-set! spec-mask n #t)
         (spec-tag spec #t)]
        [(list impl args ...)
         (when (hash-has-key? (id->e1) impl)
           (vector-set! spec-mask n #t)
           (for ([arg (in-list args)])
             (spec-tag arg #t)))]
        [_
         (when spec?
           (vector-set! spec-mask n #t))])))

  (for ([n (in-range (batch-length batch))])
    (spec-tag n #f))
  (for ([root (in-vector (batch-roots batch))])
    (vector-set! root-mask root #t))
  (for ([node (in-vector (batch-nodes batch))]
        [root? (in-vector root-mask)]
        [spec? (in-vector spec-mask)]
        [n (in-naturals)])
    (define node*
      (match node
        [(literal v repr)
         `(,(typed-num-id repr) (bigrat (from-string ,(number->string (numerator v)))
                                        (from-string ,(number->string (denominator v)))))]
        [(? number?)
         `(Num (bigrat (from-string ,(number->string (numerator node)))
                       (from-string ,(number->string (denominator node)))))]
        [(? symbol?) #f]
        [`(if ,cond ,ift ,iff)
         `(,(if spec? 'If 'IfTy) ,(remap cond spec?) ,(remap ift spec?) ,(remap iff spec?))]
        [(approx spec impl) `(Approx ,(remap spec #t) ,(remap impl #f))]
        [(list impl args ...)
         `(,(hash-ref (if spec?
                          (id->e1)
                          (id->e2))
                      impl)
           ,@(for/list ([arg (in-list args)])
               (remap arg spec?)))]))

    (if node*
        (vector-set! mappings n (insert-node! node* n root?))
        (hash-set! vars n node))
    (when root?
      (set! root-bindings (cons (vector-ref mappings n) root-bindings))))

  ; Var rules
  (define var-lowering-rules
    (for/list ([var (in-list (context-vars ctx))]
               [repr (in-list (context-var-reprs ctx))])
      `(rule ((= e (Var ,(symbol->string var))))
             ((let ty ,(symbol->string (representation-name repr))
                )
              (let ety (,(typed-var-id (representation-name repr))
                        ,(symbol->string var))
                )
              (union (lower e ty) ety))
             :ruleset
             lowering)))

  (set! egglog-exprs (append egglog-exprs var-lowering-rules))

  (define var-lifting-rules
    (for/list ([var (in-list (context-vars ctx))]
               [repr (in-list (context-var-reprs ctx))])
      `(rule ((= e (,(typed-var-id (representation-name repr)) ,(symbol->string var))))
             ((let se (Var
                       ,(symbol->string var))
                )
              (union (lift e) se))
             :ruleset
             lifting)))

  (set! egglog-exprs (append egglog-exprs var-lifting-rules))

  (define var-spec-bindings
    (for/list ([var (in-list (context-vars ctx))])
      `(let ,(string->symbol (format "?~a" var)) (Var ,(symbol->string var)))))

  (set! egglog-exprs (append egglog-exprs var-spec-bindings))

  (define var-typed-bindings
    (for/list ([var (in-list (context-vars ctx))]
               [repr (in-list (context-var-reprs ctx))])
      `(let ,(string->symbol (format "?t~a" var))
         (,(typed-var-id (representation-name repr)) ,(symbol->string var)))))
  (set! egglog-exprs (append egglog-exprs var-typed-bindings))

  (define binding-exprs
    (for/list ([root? (in-vector root-mask)]
               [n (in-naturals)]
               #:when (not (hash-has-key? vars n)))
      (define binding
        (if root?
            (string->symbol (format "?r~a" n))
            (string->symbol (format "?b~a" n))))
      `(let ,binding ,(hash-ref bindings binding))))

  (define extract-bindings
    (for/list ([root (batch-roots batch)])
      (if (hash-has-key? vars root)
          (if (vector-ref spec-mask root)
              (string->symbol (format "?~a" (hash-ref vars root)))
              (string->symbol (format "?t~a" (hash-ref vars root))))
          (string->symbol (format "?r~a" root)))))

  (set! egglog-exprs (append egglog-exprs binding-exprs))

  (cons egglog-exprs extract-bindings))

(define (egglog-num? id)
  (string-prefix? (symbol->string id) "Num"))

(define (egglog-num-repr id)
  (string->symbol (substring (symbol->string id) 3)))

(define (egglog-var? id)
  (string-prefix? (symbol->string id) "Var"))

(define (egglog-expr-typed? expr)
  (match expr
    [(? number?) #t]
    [(? variable?) #t]
    [`(,impl ,args ...) (and (not (eq? impl 'typed-id)) (andmap egglog-expr-typed? args))]))

(define (e1->expr expr)
  (let loop ([expr expr])
    (match expr
      [`(,(? egglog-num? num) (bigrat (from-string ,n) (from-string ,d)))
       (/ (string->number n) (string->number d))]
      [`(,(? egglog-var? var) ,v) (string->symbol v)]
      [`(If ,cond ,ift ,iff)
       `(if ,(loop cond)
            ,(loop ift)
            ,(loop iff))]
      [`(,op ,args ...) `(,(hash-ref (e1->id) op) ,@(map loop args))])))

(define (e2->expr expr)
  (let loop ([expr expr])
    (match expr
      [`(,(? egglog-num? num) (bigrat (from-string ,n) (from-string ,d)))
       (/ (string->number n) (string->number d))]
      [`(,(? egglog-var? var) ,v) (string->symbol v)]
      [`(IfTy ,cond ,ift ,iff)
       `(if ,(loop cond)
            ,(loop ift)
            ,(loop iff))]
      [`(Approx ,spec ,impl) (approx (e1->expr spec) (loop impl))] ;;; todo approx bug or not?
      [`(,impl ,args ...) `(,(hash-ref (e2->id) impl) ,@(map loop args))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing API
;; Most calls should be done for testing purposes through these two methods
;;  - `populate-e->id-tables`: Used for testing e1-> expr and e2-> expr.
;;                             Populates the e1->id and e2->id tables
;;  - `run-egglog`: takes an egglog runner and performs an extraction (exprs or proof)

(define (populate-e->id-tables)
  (begin

    (for ([op (in-list (all-operators))])
      (hash-set! (e1->id) (serialize-op op) op))

    (for-each (λ (pair) (hash-set! (e2->id) (car pair) (cdr pair)))
              '((Acosf32Ty . acos.f32) (Acosf64Ty . acos.f64)
                                       (Acoshf32Ty . acosh.f32)
                                       (Acoshf64Ty . acosh.f64)
                                       (Addf32Ty . +.f32)
                                       (Addf64Ty . +.f64)
                                       (AndboolTy . and.bool)
                                       (Asinf32Ty . asin.f32)
                                       (Asinf64Ty . asin.f64)
                                       (Asinhf32Ty . asinh.f32)
                                       (Asinhf64Ty . asinh.f64)
                                       (Atan2f32Ty . atan2.f32)
                                       (Atan2f64Ty . atan2.f64)
                                       (Atanf32Ty . atan.f32)
                                       (Atanf64Ty . atan.f64)
                                       (Atanhf32Ty . atanh.f32)
                                       (Atanhf64Ty . atanh.f64)
                                       (Cbrtf32Ty . cbrt.f32)
                                       (Cbrtf64Ty . cbrt.f64)
                                       (Ceilf32Ty . ceil.f32)
                                       (Ceilf64Ty . ceil.f64)
                                       (Copysignf32Ty . copysign.f32)
                                       (Copysignf64Ty . copysign.f64)
                                       (Cosf32Ty . cos.f32)
                                       (Cosf64Ty . cos.f64)
                                       (Coshf32Ty . cosh.f32)
                                       (Coshf64Ty . cosh.f64)
                                       (Divf32Ty . /.f32)
                                       (Divf64Ty . /.f64)
                                       (Ef32Ty . E.f32)
                                       (Ef64Ty . E.f64)
                                       (Eqf32Ty . ==.f32)
                                       (Eqf64Ty . ==.f64)
                                       (Erfcf32Ty . erfc.f32)
                                       (Erfcf64Ty . erfc.f64)
                                       (Erff32Ty . erf.f32)
                                       (Erff64Ty . erf.f64)
                                       (Exp2f32Ty . exp2.f32)
                                       (Exp2f64Ty . exp2.f64)
                                       (Expf32Ty . exp.f32)
                                       (Expf64Ty . exp.f64)
                                       (Expm1f32Ty . expm1.f32)
                                       (Expm1f64Ty . expm1.f64)
                                       (Fabsf32Ty . fabs.f32)
                                       (Fabsf64Ty . fabs.f64)
                                       (FalseboolTy . FALSE.bool)
                                       (Fdimf32Ty . fdim.f32)
                                       (Fdimf64Ty . fdim.f64)
                                       (Floorf32Ty . floor.f32)
                                       (Floorf64Ty . floor.f64)
                                       (Fmaf32Ty . fma.f32)
                                       (Fmaf64Ty . fma.f64)
                                       (Fmaxf32Ty . fmax.f32)
                                       (Fmaxf64Ty . fmax.f64)
                                       (Fminf32Ty . fmin.f32)
                                       (Fminf64Ty . fmin.f64)
                                       (Fmodf32Ty . fmod.f32)
                                       (Fmodf64Ty . fmod.f64)
                                       (Gtef32Ty . >=.f32)
                                       (Gtef64Ty . >=.f64)
                                       (Gtf32Ty . >.f32)
                                       (Gtf64Ty . >.f64)
                                       (Hypotf32Ty . hypot.f32)
                                       (Hypotf64Ty . hypot.f64)
                                       (Infinityf32Ty . INFINITY.f32)
                                       (Infinityf64Ty . INFINITY.f64)
                                       (Lgammaf32Ty . lgamma.f32)
                                       (Lgammaf64Ty . lgamma.f64)
                                       (Log10f32Ty . log10.f32)
                                       (Log10f64Ty . log10.f64)
                                       (Log1pf32Ty . log1p.f32)
                                       (Log1pf64Ty . log1p.f64)
                                       (Log2f32Ty . log2.f32)
                                       (Log2f64Ty . log2.f64)
                                       (Logbf32Ty . logb.f32)
                                       (Logbf64Ty . logb.f64)
                                       (Logf32Ty . log.f32)
                                       (Logf64Ty . log.f64)
                                       (Ltef32Ty . <=.f32)
                                       (Ltef64Ty . <=.f64)
                                       (Ltf32Ty . <.f32)
                                       (Ltf64Ty . <.f64)
                                       (Mulf32Ty . *.f32)
                                       (Mulf64Ty . *.f64)
                                       (Nanf32Ty . NAN.f32)
                                       (Nanf64Ty . NAN.f64)
                                       (Negf32Ty . neg.f32)
                                       (Negf64Ty . neg.f64)
                                       (Neqf32Ty . !=.f32)
                                       (Neqf64Ty . !=.f64)
                                       (NotboolTy . not.bool)
                                       (OrboolTy . or.bool)
                                       (Pif32Ty . PI.f32)
                                       (Pif64Ty . PI.f64)
                                       (Powf32Ty . pow.f32)
                                       (Powf64Ty . pow.f64)
                                       (Remainderf32Ty . remainder.f32)
                                       (Remainderf64Ty . remainder.f64)
                                       (Rintf32Ty . rint.f32)
                                       (Rintf64Ty . rint.f64)
                                       (Roundf32Ty . round.f32)
                                       (Roundf64Ty . round.f64)
                                       (Sinf32Ty . sin.f32)
                                       (Sinf64Ty . sin.f64)
                                       (Sinhf32Ty . sinh.f32)
                                       (Sinhf64Ty . sinh.f64)
                                       (Sqrtf32Ty . sqrt.f32)
                                       (Sqrtf64Ty . sqrt.f64)
                                       (Subf32Ty . -.f32)
                                       (Subf64Ty . -.f64)
                                       (Tanf32Ty . tan.f32)
                                       (Tanf64Ty . tan.f64)
                                       (Tanhf32Ty . tanh.f32)
                                       (Tanhf64Ty . tanh.f64)
                                       (Tgammaf32Ty . tgamma.f32)
                                       (Tgammaf64Ty . tgamma.f64)
                                       (TrueboolTy . TRUE.bool)
                                       (Truncf32Ty . trunc.f32)
                                       (Truncf64Ty . trunc.f64)))))