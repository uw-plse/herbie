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
         egglog-add-exprs
        ;  run-egglog-process
        ;  (struct-out egglog-program)
         make-egglog-runner
         run-egglog-multi-extractor
         run-egglog-proofs
         run-egglog-equal?)

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

;; TODO : prelude, rules, expressions, extractions
; (struct egglog-program (program) #:prefab)

; Types handled
; - rationals
; - string
(define (process-egglog program)
  (define egglog-file-path
    (let ([temp-file (make-temporary-file "program-to-egglog-~a.egg")])
      (with-output-to-file temp-file #:exists 'replace (lambda () (for-each writeln program)))
      temp-file))

  (define egglog-path
    (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

  (define stdout-port (open-output-string))
  (define stderr-port (open-output-string))

  ; (printf "egglog-path : ~a\n" egglog-path)
  ; (printf "egglog-file-path : ~a\n\n" egglog-file-path)

  (define old-error-port (current-error-port))

  ;; Run egglog and capture output
  (parameterize ([current-output-port stdout-port]
                 [current-error-port stderr-port])
    (unless (system (format "~a ~a" egglog-path egglog-file-path))
      (begin
        (fprintf old-error-port "stdout-port ~a\n" (get-output-string stdout-port))
        (fprintf old-error-port "stderr-port ~a\n" (get-output-string stderr-port))
        (error "Failed to execute egglog"))))

  ; (delete-file egglog-file-path)

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
  (define program '())

  (define curr-program (new egglog-program%))

  (send curr-program add! (prelude #:mixed-egraph? #t))
  (set! program (append program (prelude #:mixed-egraph? #t)))

  (printf "prev program ~a\n\n" program)
  (printf "curr program ~a\n\n" (send curr-program get-current-program-list))


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
           ;; Add rulsets
           (set! program (append program `((ruleset ,curr-tag))))
           (send curr-program add! `((ruleset ,curr-tag)))

           ;; Add the actual egglog rewrite rules
           (set! program (append program (egglog-rewrite-rules rule-type curr-tag)))
           (send curr-program add! (egglog-rewrite-rules rule-type curr-tag))

           curr-tag]))

      (cons tag schedule-params)))

  ; (printf "curr program ~a\n\n" (send curr-program get-current-program-list))


  ;; 3. Inserting expressions -> (egglog-add-exprs curr-batch (egglog-runner-ctx))
  ; (exprs . extract bindings)
  (define egglog-batch-exprs (egglog-add-exprs curr-batch (egg-runner-ctx runner)))

  (set! program (append program (car egglog-batch-exprs)))
  (send curr-program add! (car egglog-batch-exprs))
  ; (printf "curr program ~a\n\n" (send curr-program get-current-program-list))


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

  (set! program (append program `((run-schedule ,@run-schedule))))
  (send curr-program add! `((run-schedule ,@run-schedule)))
  ; (printf "curr program ~a\n\n" (send curr-program get-current-program-list))


  ;; 5. Extraction -> should just need root ids
  (for ([binding (cdr egglog-batch-exprs)])
    (begin
      (define val
        (if num-variants
            (list `(extract ,binding 5))

            (match domain-fns
              [(list 'lifting) (list `(extract (lift ,binding)))]
              [(list 'lowering)
              (define curr-val
                (symbol->string (representation-name (context-repr (egg-runner-ctx runner)))))
              (list `(extract (lower ,binding ,curr-val)))]
              [_ (list `(extract ,binding))])))
              
              
      (set! program (append program val))
      (send curr-program add! val)))
      
  ; (define egglog-output (run-egglog-process (egglog-program program)))

  (printf "PREV program ~a\n\n" program)

  (define egglog-output (send curr-program process-egglog))
  (define stdout-content (car egglog-output))
  (define stderr-content (cdr egglog-output))

  (define input-batch (egg-runner-batch runner))
  (define out (batch->mutable-batch input-batch))

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
; ; TODO: if i have  two expressions how di i know if they are in the same e-class
; ; if we are outside of egglog
(define (run-egglog-equal? runner expr-pairs) ; term equality?
  (for/list ([i (in-range (length expr-pairs))])
    #f))

  ; (define program '())

  ; ;; 1. Prelude
  ; (set! program (append program (prelude #:mixed-egraph? #t)))

  ; ;; 2. User Rules which comes from schedule (need to be translated)
  ; (define tag-schedule
  ;   (for/list ([i (in-naturals 1)] ; Start index `i` from 1
  ;              [element (in-list (egg-runner-schedule runner))])

  ;     (define rule-type (car element))
  ;     (define schedule-params (cdr element))

  ;     (define curr-tag (string->symbol (string-append "?tag" (number->string i))))

  ;     ;; Add rulsets
  ;     (set! program (append program `((ruleset ,curr-tag))))

  ;     ;; Add the actual egglog rewrite rules
  ;     (set! program (append program (egglog-rewrite-rules rule-type curr-tag)))

  ;     (cons curr-tag schedule-params)))
  ; (for ([(start-expr end-expr) (in-dict expr-pairs)]
  ;       [i (in-naturals 1)])
  ;   (define start-let
  ;     `(let ,(string->symbol (string-append "?e1" (number->string i))) ,(expr->e1-expr start-expr)))
  ;   (define end-let
  ;     `(let ,(string->symbol (string-append "?e2" (number->string i))) ,(expr->e1-expr end-expr)))
  ;   (set! program (append program `(,start-let ,end-let))))

  ; ;; 3. Running the schedule
  ; (set! program (append program '((run-schedule (repeat 3 ?tag1) (repeat 20 const-fold)))))

  ; ;; Running Checks
  ; (for ([(start-expr end-expr) (in-dict expr-pairs)]
  ;       [i (in-naturals 1)])
  ;   (define start-extract `(extract ,(string->symbol (string-append "?e1" (number->string i)))))
  ;   (define end-extract `(extract ,(string->symbol (string-append "?e2" (number->string i)))))
  ;   (set! program (append program `(,start-extract ,end-extract))))

  ; ;; 6. Call run-egglog-process
  ; (define egglog-output (run-egglog-process (egglog-program program)))
  ; (define stdout-content (car egglog-output))

  ; (define extract-results (list->vector (string-split stdout-content "\n")))
  ; (define stderr-content (cdr egglog-output))

  ; (for/list ([i (in-range 0 (vector-length extract-results) 2)])
  ;   (equal? (vector-ref extract-results i) (vector-ref extract-results (+ i 1)))))

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
               (Approx M MTy)
               ,@(platform-impl-nodes pform)))
  (set! prelude-exprs (append prelude-exprs (list typed-graph)))
  (define lower-fn `(function lower (M String) MTy :unextractable))
  (set! prelude-exprs (append prelude-exprs (list lower-fn)))
  (define lift-fn `(function lift (MTy) M :unextractable))
  (set! prelude-exprs (append prelude-exprs (list lift-fn)))
  (set! prelude-exprs
        (append prelude-exprs (list `(ruleset const-fold) `(ruleset lowering) `(ruleset lifting))))
  (set! prelude-exprs (append prelude-exprs const-fold))
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

(define (egglog-add-exprs batch ctx)
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
      [`(Approx ,spec ,impl) (approx (e1->expr spec) (loop impl))]
      [`(,impl ,args ...) `(,(hash-ref (e2->id) impl) ,@(map loop args))])))
