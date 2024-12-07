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
         "egg-herbie.rkt")

(provide prelude
         egglog-add-exprs
         run-egglog-process
         (struct-out egglog-program)
         make-egglog-runner
         run-egglog-single-extractor
         run-egglog-multi-extractor
         run-egglog-proofs
         run-egglog-equal?)

(module+ test
  (require rackunit)
  (require "../syntax/load-plugin.rkt")
  (load-herbie-builtins))

(define op-string-names
  (hash '+ 'Add '- 'Sub '* 'Mul '/ 'Div '== 'Eq '!= 'Neq '> 'Gt '< 'Lt '>= 'Gte '<= 'Lte))

(define id->e1 (make-hasheq))
(define e1->id (make-hasheq))
(define id->e2 (make-hasheq))
(define e2->id (make-hasheq))

(define file-num 1)

;; [Copied from egg-herbie.rkt] Returns all representatations (and their types) in the current platform.
(define (all-repr-names [pform (*active-platform*)])
  (remove-duplicates (map (lambda (repr) (representation-name repr)) (platform-reprs pform))))

;; Track the entire Egglog program in one go by "converting" into racket based code
;; TODO : prelude, rules, expressions, extractions
(struct egglog-program (program) #:prefab)

(define program-to-egglog "program-to-egglog.egg")

; Types handled
; - rationals
; - string
(define (write-program-to-egglog program)
  (with-output-to-file program-to-egglog #:exists 'replace (lambda () (for-each writeln program))))

; (define (process-egglog egglog-filename)
;   (define egglog-path
;     (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

;   (define curr-path (build-path (current-directory) egglog-filename))

;   (define-values (sp out in err) (subprocess #f #f #f egglog-path curr-path))

;   (subprocess-wait sp)

;   (define stdout-content (port->string out))
;   (define stderr-content (port->string err))

;   (close-input-port out)
;   (close-output-port in)
;   (close-input-port err)

;   (printf "reached here \n")

;   (cons stdout-content stderr-content))

(define (process-egglog egglog-filename)
  (define egglog-path
    (path->string (or (find-executable-path "egglog") (error "egglog executable not found in PATH"))))

  (define curr-path (path->string (build-path (current-directory) egglog-filename)))

  (let ([stdout-port (open-output-string)]
        [stderr-port (open-output-string)])

    (parameterize ([current-output-port stdout-port]
                   [current-error-port stderr-port])
      (system (string-append egglog-path " " curr-path)))

    (cons (get-output-string stdout-port) (get-output-string stderr-port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;
;; High-level function that writes the program to a file, runs it then returns output
(define (run-egglog-process program-struct)
  (write-program-to-egglog (egglog-program-program program-struct))

  (process-egglog program-to-egglog))

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
           [(cons 'node (? nonnegative-integer?)) (void)] ; exists (node 5)
           [(cons 'iteration (? nonnegative-integer?)) (void)] ; exists (run 5)
           [(cons 'const-fold? (? boolean?)) (void)] ; AVOID
           [(cons 'scheduler mode) ; AVOID
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

;; TODO : Need to run egglog to get the actual ids per
(define (run-egglog-single-extractor runner extractor) ; single expression extraction
  (define curr-batch (egg-runner-batch runner)) ; to do switch to egglog

  ;; 1. make the program
  ;; requires prelude -> list of exprs (only based on platform)
  ; (define prelude-exprs (prelude #:mixed-egraph? #t))

  ;; eglog has ssyntax for which ruleset a rule belongs to

  ;; when making schedule, direct which specific ruleset to run
  ;; basically an expr
  ;; run-schedule

  ;; need to translate all rules of runner
  ; (define additional-rules (map cons (egg-runner-schedule runner)))

  ; (define program (append prelude-exprs additional-rules))

  ;; egglog-add-exprs : batch ctx -> listof exprs
  ; (egglog-add-exprs curr-batch (egglog-runner-ctx))

  ;; 3. Construct the schedule

  ; (ruleset math)
  ; (ruleset fp-safe)
  ; (egglog-rewrite-rules)

  ; lift and lower is already in prelude
  ; all other rules have to be translated and appended

  ; Have to translate rules with ruleset, tag and params???
  ; expressions to extract -> iterate over roots

  ;; 2. Call process-egglog

  ;; 3. Parse output

  ;; Structure -> Genrate separately and append one after the other
  (define program '())

  ;; 1. Prelude
  (set! program (append program (prelude #:mixed-egraph? #t)))

  ; (printf "before tag\n")
  ;; 2. User Rules which comes from schedule (need to be translated)

  (define tag-schedule
    (for/list ([i (in-naturals 1)] ; Start index `i` from 1
               [element (in-list (egg-runner-schedule runner))])

      (define rule-type (car element))
      (define schedule-params (cdr element))

      (define tag
        (match rule-type
          ['lift 'lifting]
          ['lower 'lowering]
          [_
           (define curr-tag (string->symbol (string-append "?tag" (number->string i))))

           ;; Add rulsets
           (set! program (append program `((ruleset ,curr-tag))))

           ;; Add the actual egglog rewrite rules
           (set! program (append program (egglog-rewrite-rules rule-type curr-tag)))

           curr-tag]))

      (cons tag schedule-params)))
    
  ; (printf "after tag\n")

  ;; 3. Inserting expressions -> (egglog-add-exprs curr-batch (egglog-runner-ctx))
  (set! program (append program (egglog-add-exprs curr-batch (egg-runner-ctx runner))))

  ;; 4. Running the schedule

  ; `((run-schedule (saturate lifting) (saturate math) (saturate lowering) ))
  (define run-schedule '())

  (for ([(tag schedule-params) (in-dict tag-schedule)])
    (match tag
      [(or 'lifting 'lowering) (set! run-schedule (append run-schedule (list (list 'saturate tag))))]
      [_
        ; Set Tag
        ; (set! run-schedule (append run-schedule (list (list 'saturate tag))))

        ; Set params
        (define is-node-present (dict-ref schedule-params 'node #f))
        (define is-iteration-present (dict-ref schedule-params 'iteration #f))
        
        (match* (is-node-present is-iteration-present)
          [((? nonnegative-integer? node-amt) (? nonnegative-integer? iter-amt))
            (set! run-schedule (append run-schedule `((repeat ,iter-amt ,tag))))]
            
          [(#f (? nonnegative-integer? iter-amt))
            (set! run-schedule (append run-schedule `((repeat ,iter-amt ,tag))))]

          [((? nonnegative-integer? node-amt) #f)
            (set! run-schedule (append run-schedule `((repeat 1 ,tag))))]

         [(#f #f) `((repeat 1 ,tag))])
         
         ]))

  ; (printf "run-schedule ~a \n\n" run-schedule)

  (set! program (append program `((run-schedule ,@run-schedule))))
  ; (set! program (append program `((run 10))))
  ; (set! program (append program 
  ; `((run-schedule
  ;   (saturate ?tag1)
  ;   (run ?tag1 4))))

  ;; dict-ref
  ; (printf "finished run-schedule \n")

  ;; 5. Extraction -> should just need root ids
  (for ([root (egg-runner-roots runner)])
    (define root-tag (string->symbol (string-append "?r" (number->string root))))

    (set! program
      (append program
        (list `(extract ,root-tag)))))
  ; TODO : not only binary64
  
  ;; 6. Call run-egglog-process
  (define egglog-output (run-egglog-process (egglog-program program)))
  (define stdout-content (car egglog-output))
  (with-output-to-file 
    (string-append "stdout" (string-append (number->string file-num) ".txt")) 
    #:exists 'replace (lambda () (writeln stdout-content)))
  (printf "stdout program: \n ~a \n\n" stdout-content)
  (define stderr-content (cdr egglog-output))
  ; (with-output-to-file "stderr.txt" #:exists 'replace (lambda () (writeln stderr-content)))
  (with-output-to-file 
    (string-append "stderr" (string-append (number->string file-num) ".txt")) 
    #:exists 'replace (lambda () (writeln stderr-content)))

  (set! file-num (+ file-num 1))
  ; (printf "stderr program: \n ~a \n\n" stderr-content)

  ;; 7. Parse output

  ; (printf "reached end\n")

  ;; (Listof (Listof batchref))
  (define out
    (for/list ([root (batch-roots curr-batch)])
      (list (batchref curr-batch root))))

  out)

;; TODO : Need to run egglog to get the actual ids
;; very hard - per id recruse one level and ger simplest child
(define (run-egglog-multi-extractor runner extractor) ; multi expression extraction
  (define curr-batch (egg-runner-batch runner))

  ;; (Listof (Listof batchref))
  (define out
    (for/list ([root (batch-roots curr-batch)])
      (list (batchref curr-batch root))))

  out)

;; egglog does not have proof
;; there is some value that herbie has which indicates we could not
;; find a proof. Might be (list #f #f ....)
(define (run-egglog-proofs runner rws) ; proof extraction
  (for/list ([(start-expr end-expr) (in-dict rws)])
    #f))

; ; 1. ask within egglog program what is id
; ; 2. Extract expression from each expr
; ; TODO: if i have  two expressions how di i know if they are in the same e-class
; ; if we are outside of egglog
(define (run-egglog-equal? runner expr-pairs) ; term equality?
  (for/list ([(start-expr end-expr) (in-dict expr-pairs)])
    #f))

(define (prelude #:mixed-egraph? [mixed-egraph? #t])
  (load-herbie-builtins)
  (define pform (*active-platform*))
  (define prelude-exprs '())
  (define spec-egraph
    `(datatype M
               (Num BigRat :cost 4294967295)
               (Var String :cost 4294967295)
               (If M M M :cost 4294967295)
               ,@(platform-spec-nodes)))
  (set! prelude-exprs (append prelude-exprs (list spec-egraph)))
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
               (Approx M MTy :cost 0)
               ,@(platform-impl-nodes pform)))
  (set! prelude-exprs (append prelude-exprs (list typed-graph)))
  (define lower-fn `(function lower (M String) MTy))
  (set! prelude-exprs (append prelude-exprs (list lower-fn)))
  (define lift-fn `(function lift (MTy) M :unextractable))
  (set! prelude-exprs (append prelude-exprs (list lift-fn)))
  (set! prelude-exprs
        (append prelude-exprs
                (list `(ruleset lowering) `(ruleset lifting) `(ruleset math) `(ruleset fp-safe))))
  (define impl-lowering (impl-lowering-rules pform))
  (set! prelude-exprs (append prelude-exprs impl-lowering))
  (define impl-lifting (impl-lifting-rules pform))
  (set! prelude-exprs (append prelude-exprs impl-lifting))
  (define num-lowering (num-lowering-rules))
  (set! prelude-exprs (append prelude-exprs num-lowering))
  (define num-lifting (num-lifting-rules))
  (set! prelude-exprs (append prelude-exprs num-lifting))
  (define if-lowering (if-lowering-rules))
  (set! prelude-exprs (append prelude-exprs if-lowering))
  (define if-lifting (if-lifting-rule))
  (set! prelude-exprs (append prelude-exprs (list if-lifting)))
  (define approx-lifting (approx-lifting-rule))
  (set! prelude-exprs (append prelude-exprs (list approx-lifting)))
  prelude-exprs)

(define (platform-spec-nodes)
  (for/list ([op (in-list (all-operators))])
    (hash-set! id->e1 op (serialize-op op))
    (hash-set! e1->id (serialize-op op) op)
    (define arity (length (operator-info op 'itype)))
    `(,(serialize-op op) ,@(for/list ([i (in-range arity)])
                             'M)
                         :cost
                         4294967295)))

(define (platform-impl-nodes pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define arity (length (impl-info impl 'itype)))
    (define typed-name (string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty")))
    (hash-set! id->e2 impl typed-name)
    (hash-set! e2->id typed-name impl)
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
       `(,(hash-ref (if (hash-has-key? id->e1 op) id->e1 id->e2) op) ,@(map loop args))])))

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
      [(? symbol?) `(,(typed-var-id (representation-name repr)) ,expr)]
      [`(if ,cond ,ift ,iff) `(IfTy ,(loop cond repr) ,(loop ift repr) ,(loop iff repr))]
      [(list op args ...)
       `(,(hash-ref id->e2 op) ,@(for/list ([arg (in-list args)]
                                            [itype (in-list (impl-info op 'itype))])
                                   (loop arg itype)))])))

(define (expr->e1-pattern expr)
  (let loop ([expr expr])
    (match expr
      [(? number?)
       `(Num (bigrat (from-string ,(number->string (numerator expr)))
                     (from-string ,(number->string (denominator expr)))))]
      [(? symbol?) `(Var ,expr)]
      [`(if ,cond ,ift ,iff) `(If ,(loop cond) ,(loop ift) ,(loop iff))]
      [(list op args ...) `(,(hash-ref id->e1 op) ,@(map loop args))])))

(define (egglog-rewrite-rules rules tag)
  (for/list ([rule (in-list rules)])
    ; (printf "rule ~a\n" rule)
    ; (printf "input ~a\n" (rule-input rule))
    ; (printf "output ~a\n" (rule-output rule))
    ; (printf "o-type ~a\n\n" (rule-otype rule))

    (if (not (representation? (rule-otype rule)))
        `(rewrite ,(expr->e1-pattern (rule-input rule))
                  ,(expr->e1-pattern (rule-output rule))
                  :ruleset
                  ,tag)
        `(rewrite ,(expr->e2-pattern (rule-input rule) (rule-otype rule))
                  ,(expr->e2-pattern (rule-output rule) (rule-otype rule))
                  :ruleset
                  ,tag))))

(define (egglog-add-exprs batch ctx)
  (define egglog-exprs '())
  (define insert-batch (batch-remove-zombie batch (batch-roots batch)))
  (define mappings (build-vector (batch-length insert-batch) values))
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
  (define root-mask (make-vector (batch-length insert-batch) #f))
  (define spec-mask (make-vector (batch-length insert-batch) #f))
  (define (spec-tag n spec?)
    (when (not (vector-ref spec-mask n))
      (match (vector-ref (batch-nodes insert-batch) n)
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
         (when (hash-has-key? id->e1 impl)
           (vector-set! spec-mask n #t)
           (for ([arg (in-list args)])
             (spec-tag arg #t)))]
        [_
         (when spec?
           (vector-set! spec-mask n #t))])))

  (for ([n (in-range (batch-length insert-batch))])
    (spec-tag n #f))
  (for ([root (in-vector (batch-roots insert-batch))])
    (vector-set! root-mask root #t))
  (for ([node (in-vector (batch-nodes insert-batch))]
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
        [(approx spec impl) `(Approx ,(remap spec spec?) ,(remap impl spec?))]
        [(list impl args ...)
         `(,(hash-ref (if spec? id->e1 id->e2) impl) ,@(for/list ([arg (in-list args)])
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

  (set! egglog-exprs (append egglog-exprs binding-exprs))

  egglog-exprs)

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
      [`(,op ,args ...) `(,(hash-ref e1->id op) ,@(map loop args))])))

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
      [`(,impl ,args ...) `(,(hash-ref e2->id impl) ,@(map loop args))])))
