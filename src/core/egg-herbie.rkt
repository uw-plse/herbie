#lang racket

(require "../../egg-herbie/main.rkt"
         (only-in ffi/vector
                  make-u32vector
                  u32vector-length
                  u32vector-set!
                  u32vector-ref
                  list->u32vector
                  u32vector->list)
         json) ; for dumping

(require "programs.rkt"
         "rules.rkt"
         "../syntax/matcher.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../config.rkt"
         "../utils/timeline.rkt"
         "batch.rkt")

(provide (struct-out egg-runner)
         typed-egg-batch-extractor
         platform-egg-cost-proc
         default-egg-cost-proc
         make-egg-runner
         run-egg
         get-canon-rule-name
         remove-rewrites)

(module+ test
  (require rackunit)
  (require "../syntax/load-plugin.rkt")
  (load-herbie-builtins))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FFI utils

(define (u32vector-empty? x)
  (zero? (u32vector-length x)))

(define (in-u32vector vec)
  (make-do-sequence
   (lambda ()
     (define len (u32vector-length vec))
     (values (lambda (i) (u32vector-ref vec i)) add1 0 (lambda (i) (< i len)) #f #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; egg FFI shim
;;
;; egg-herbie requires a bit of nice wrapping
;; - FFIRule: struct defined in egg-herbie
;; - EgraphIter: struct defined in egg-herbie

;; Wrapper around Rust-allocated egg runner
(struct egraph-data
        (egraph-pointer ; FFI pointer to runner
         herbie->egg-dict ; map from symbols to canonicalized names
         egg->herbie-dict ; inverse map
         id->spec)) ; map from e-class id to an approx-spec or #f

; Makes a new egraph that is managed by Racket's GC
(define (make-egraph mode)
  (if (equal? mode 'single)
    (egraph-data (egraph_create 0) (make-hash) (make-hash) (make-hash))
    (egraph-data (egraph_create 1) (make-hash) (make-hash) (make-hash))))

; Creates a new runner using an existing egraph.
; Useful for multi-phased rule application
(define (egraph-copy eg-data)
  (struct-copy egraph-data
               eg-data
               [egraph-pointer (egraph_copy (egraph-data-egraph-pointer eg-data))]))

; Adds expressions returning the root ids
(define (egraph-add-exprs egg-data batch roots ctx)
  (match-define (egraph-data ptr herbie->egg-dict egg->herbie-dict id->spec) egg-data)

  ; lookups the egg name of a variable
  (define (normalize-var x)
    (hash-ref! herbie->egg-dict
               x
               (lambda ()
                 (define id (hash-count herbie->egg-dict))
                 (define replacement (string->symbol (format "$h~a" id)))
                 (hash-set! egg->herbie-dict replacement (cons x (context-lookup ctx x)))
                 replacement)))

  ; normalizes an approx spec
  (define (normalize-spec expr)
    (match expr
      [(? number?) expr]
      [(? symbol?) (normalize-var expr)]
      [(list op args ...) (cons op (map normalize-spec args))]))

  ; pre-allocated id vectors for all the common cases
  (define 0-vec (make-u32vector 0))
  (define 1-vec (make-u32vector 1))
  (define 2-vec (make-u32vector 2))
  (define 3-vec (make-u32vector 3))

  (define (list->u32vec xs)
    (match xs
      [(list) 0-vec]
      [(list x)
       (u32vector-set! 1-vec 0 x)
       1-vec]
      [(list x y)
       (u32vector-set! 2-vec 0 x)
       (u32vector-set! 2-vec 1 y)
       2-vec]
      [(list x y z)
       (u32vector-set! 3-vec 0 x)
       (u32vector-set! 3-vec 1 y)
       (u32vector-set! 3-vec 2 z)
       3-vec]
      [_ (list->u32vector xs)]))

  ; node -> natural
  ; inserts an expression into the e-graph, returning its e-class id.
  (define (insert-node! node root?)
    (match node
      [(list op ids ...) (egraph_add_node ptr (symbol->string op) (list->u32vec ids) root?)]
      [(? symbol? x) (egraph_add_node ptr (symbol->string x) 0-vec root?)]
      [(? number? n) (egraph_add_node ptr (number->string n) 0-vec root?)]))

  (define insert-batch (batch-remove-zombie batch roots))

  (define mappings (build-vector (batch-length insert-batch) values))
  (define (remap x)
    (vector-ref mappings x))

  ; Inserting nodes bottom-up
  (define root-mask (make-vector (batch-length insert-batch) #f))
  (for ([root (in-vector (batch-roots insert-batch))])
    (vector-set! root-mask root #t))
  (for ([node (in-vector (batch-nodes insert-batch))]
        [root? (in-vector root-mask)]
        [n (in-naturals)])
    (define node*
      (match node
        [(literal v _) v]
        [(? number?) node]
        [(? symbol?) (normalize-var node)]
        [(approx spec impl)
         (hash-ref! id->spec
                    (remap spec)
                    (lambda ()
                      (define spec* (normalize-spec (batch-ref insert-batch spec)))
                      (define type (representation-type (repr-of-node insert-batch impl ctx)))
                      (cons spec* type))) ; preserved spec and type for extraction
         (list '$approx (remap spec) (remap impl))]
        [(list op (app remap args) ...) (cons op args)]))

    (vector-set! mappings n (insert-node! node* root?)))

  (for/list ([root (in-vector (batch-roots insert-batch))])
    (remap root)))

;; runs rules on an egraph (optional iteration limit)
(define (egraph-run egraph-data ffi-rules node-limit iter-limit scheduler const-folding?)
  (define u32_max 4294967295) ; since we can't send option types
  (define node_limit (if node-limit node-limit u32_max))
  (define iter_limit (if iter-limit iter-limit u32_max))
  (define simple_scheduler?
    (match scheduler
      ['backoff #f]
      ['simple #t]
      [_ (error 'egraph-run "unknown scheduler: `~a`" scheduler)]))
  (define timeline-end! (timeline-start! 'times (~a "i-egraph-run")))
  (define out (egraph_run (egraph-data-egraph-pointer egraph-data)
              ffi-rules
              iter_limit
              node_limit
              simple_scheduler?
              const-folding?))
  (timeline-end!)
  out)

(define (egraph-get-simplest egraph-data node-id iteration ctx)
  (define expr (egraph_get_simplest (egraph-data-egraph-pointer egraph-data) node-id iteration))
  (egg-expr->expr expr egraph-data (context-repr ctx)))

(define (egraph-get-variants egraph-data node-id orig-expr ctx)
  (define egg-expr (expr->egg-expr orig-expr egraph-data ctx))
  (define exprs (egraph_get_variants (egraph-data-egraph-pointer egraph-data) node-id egg-expr))
  (for/list ([expr (in-list exprs)])
    (egg-expr->expr expr egraph-data (context-repr ctx))))

(define (egraph-is-unsound-detected egraph-data)
  (egraph_is_unsound_detected (egraph-data-egraph-pointer egraph-data)))

(define (egraph-get-cost egraph-data node-id iteration)
  (egraph_get_cost (egraph-data-egraph-pointer egraph-data) node-id iteration))

(define (egraph-get-times-applied egraph-data rule)
  (egraph_get_times_applied (egraph-data-egraph-pointer egraph-data) (FFIRule-name rule)))

(define (egraph-stop-reason egraph-data)
  (match (egraph_get_stop_reason (egraph-data-egraph-pointer egraph-data))
    [0 "saturated"]
    [1 "iter limit"]
    [2 "node limit"]
    [3 "unsound"]
    [sr (error 'egraph-stop-reason "unexpected stop reason ~a" sr)]))

;; Extracts the eclasses of an e-graph as a u32vector
(define (egraph-eclasses egraph-data)
  (egraph_get_eclasses (egraph-data-egraph-pointer egraph-data)))

(define empty-u32vec (make-u32vector 0))

;; Extracts the nodes of an e-class as a vector
;; where each enode is either a symbol, number, or list
(define (egraph-get-eclass egraph-data id)
  (define ptr (egraph-data-egraph-pointer egraph-data))
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (define eclass (egraph_get_eclass ptr id))
  ; need to fix up any constant operators
  (for ([enode (in-vector eclass)]
        [i (in-naturals)])
    (when (and (symbol? enode)
               (not (string-prefix? (symbol->string enode) "$h"))
               (not (hash-has-key? egg->herbie enode)))
      (vector-set! eclass i (cons enode empty-u32vec))))
  eclass)

(define (egraph-find egraph-data id)
  (egraph_find (egraph-data-egraph-pointer egraph-data) id))

(define (egraph-expr-equal? egraph-data expr goal ctx)
  (define batch (progs->batch (list expr goal)))
  (match-define (list id1 id2) (egraph-add-exprs egraph-data batch (batch-roots batch) ctx))
  (= id1 id2))

;; returns a flattened list of terms or #f if it failed to expand the proof due to budget
(define (egraph-get-proof egraph-data expr goal ctx)
  (define egg-expr (expr->egg-expr expr egraph-data ctx))
  (define egg-goal (expr->egg-expr goal egraph-data ctx))
  (define str (egraph_get_proof (egraph-data-egraph-pointer egraph-data) egg-expr egg-goal))
  (cond
    [(<= (string-length str) (*proof-max-string-length*))
     (define converted
       (for/list ([expr (in-port read (open-input-string str))])
         (egg-expr->expr expr egraph-data (context-repr ctx))))
     (define expanded (expand-proof converted (box (*proof-max-length*))))
     (if (member #f expanded) #f expanded)]
    [else #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eggIR
;;
;; eggIR is an S-expr language nearly identical to Herbie's various IRs
;; consisting of two variants:
;;  - patterns: all variables are prefixed by '?'
;;  - expressions: all variables are normalized into `h<n>` where <n> is an integer
;;

;; Translates a Herbie rule LHS or RHS into a pattern usable by egg.
;; Rules can be over specs or impls.
(define (expr->egg-pattern expr)
  (let loop ([expr expr])
    (match expr
      [(? number?) expr]
      [(? literal?) (literal-value expr)]
      [(? symbol?) (string->symbol (format "?~a" expr))]
      [(approx spec impl) (list '$approx (loop spec) (loop impl))]
      [(list op args ...) (cons op (map loop args))])))

;; Translates a Herbie expression into an expression usable by egg.
;; Updates translation dictionary upon encountering variables.
;; Result is the expression.

(define (expr->egg-expr expr egg-data ctx)
  (define egg->herbie-dict (egraph-data-egg->herbie-dict egg-data))
  (define herbie->egg-dict (egraph-data-herbie->egg-dict egg-data))
  (let loop ([expr expr])
    (match expr
      [(? number?) expr]
      [(? literal?) (literal-value expr)]
      [(? symbol?)
       (hash-ref! herbie->egg-dict
                  expr
                  (lambda ()
                    (define id (hash-count herbie->egg-dict))
                    (define replacement (string->symbol (format "$h~a" id)))
                    (hash-set! egg->herbie-dict replacement (cons expr (context-lookup ctx expr)))
                    replacement))]
      [(approx spec impl) (list '$approx (loop spec) (loop impl))]
      [(list op args ...) (cons op (map loop args))])))

(define (flatten-let expr)
  (let loop ([expr expr]
             [env (hash)])
    (match expr
      [(? number?) expr]
      [(? symbol?) (hash-ref env expr expr)]
      [`(let (,var
              ,term)
          ,body)
       (loop body (hash-set env var (loop term env)))]
      [`(,op ,args ...) (cons op (map (curryr loop env) args))])))

;; Converts an S-expr from egg into one Herbie understands
;; TODO: typing information is confusing since proofs mean
;; we may process mixed spec/impl expressions;
;; only need `type` to correctly interpret numbers
(define (egg-parsed->expr expr rename-dict type)
  (let loop ([expr expr]
             [type type])
    (match expr
      [(? number?)
       (if (representation? type)
           (literal expr (representation-name type))
           expr)]
      [(? symbol?)
       (if (hash-has-key? rename-dict expr)
           (car (hash-ref rename-dict expr)) ; variable (extract uncanonical name)
           (list expr))] ; constant function
      [(list '$approx spec impl) ; approx
       (define spec-type
         (if (representation? type)
             (representation-type type)
             type))
       (approx (loop spec spec-type) (loop impl type))]
      [`(Explanation ,body ...) `(Explanation ,@(map (lambda (e) (loop e type)) body))]
      [(list 'Rewrite=> rule expr) (list 'Rewrite=> rule (loop expr type))]
      [(list 'Rewrite<= rule expr) (list 'Rewrite<= rule (loop expr type))]
      [(list 'if cond ift iff)
       (if (representation? type)
           (list 'if (loop cond (get-representation 'bool)) (loop ift type) (loop iff type))
           (list 'if (loop cond 'bool) (loop ift type) (loop iff type)))]
      [(list (? impl-exists? impl) args ...) (cons impl (map loop args (impl-info impl 'itype)))]
      [(list op args ...) (cons op (map loop args (operator-info op 'itype)))])))

;; Parses a string from egg into a single S-expr.
(define (egg-expr->expr egg-expr egraph-data type)
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (egg-parsed->expr (flatten-let egg-expr) egg->herbie type))

(module+ test
  (define repr (get-representation 'binary64))
  (*context* (make-debug-context '()))
  (*context* (context-extend (*context*) 'x repr))
  (*context* (context-extend (*context*) 'y repr))
  (*context* (context-extend (*context*) 'z repr))

  (define test-exprs
    (list (cons '(+.f64 y x) '(+.f64 $h0 $h1))
          (cons '(+.f64 x y) '(+.f64 $h1 $h0))
          (cons '(-.f64 #s(literal 2 binary64) (+.f64 x y)) '(-.f64 2 (+.f64 $h1 $h0)))
          (cons '(-.f64 z (+.f64 (+.f64 y #s(literal 2 binary64)) x))
                '(-.f64 $h2 (+.f64 (+.f64 $h0 2) $h1)))
          (cons '(*.f64 x y) '(*.f64 $h1 $h0))
          (cons '(+.f64 (*.f64 x y) #s(literal 2 binary64)) '(+.f64 (*.f64 $h1 $h0) 2))
          (cons '(cos.f32 (PI.f32)) '(cos.f32 (PI.f32)))
          (cons '(if (TRUE) x y) '(if (TRUE) $h1 $h0))))

  (let ([egg-graph (make-egraph)])
    (for ([(in expected-out) (in-dict test-exprs)])
      (define out (expr->egg-expr in egg-graph (*context*)))
      (define computed-in (egg-expr->expr out egg-graph (context-repr (*context*))))
      (check-equal? out expected-out)
      (check-equal? computed-in in)))

  (*context* (make-debug-context '(x a b c r)))
  (define extended-expr-list
    ; specifications
    (list '(/ (- (exp x) (exp (neg x))) 2)
          '(/ (+ (neg b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a))
          '(/ (+ (neg b) (sqrt (- (* b b) (* (* 3 a) c)))) (* 3 a))
          '(* r 30)
          '(* 23/54 r)
          '(+ 3/2 1.4)
          ; implementations
          `(/.f64 (-.f64 (exp.f64 x) (exp.f64 (neg.f64 x))) ,(literal 2 'binary64))
          `(/.f64 (+.f64 (neg.f64 b)
                         (sqrt.f64 (-.f64 (*.f64 b b) (*.f64 (*.f64 ,(literal 3 'binary64) a) c))))
                  (*.f64 ,(literal 3 'binary64) a))
          `(/.f64 (+.f64 (neg.f64 b)
                         (sqrt.f64 (-.f64 (*.f64 b b) (*.f64 (*.f64 ,(literal 3 'binary64) a) c))))
                  (*.f64 ,(literal 3 'binary64) a))
          `(*.f64 r ,(literal 30 'binary64))
          `(*.f64 ,(literal 23/54 'binary64) r)
          `(+.f64 ,(literal 3/2 'binary64) ,(literal 1.4 'binary64))))

  (let ([egg-graph (make-egraph)])
    (for ([expr extended-expr-list])
      (define egg-expr (expr->egg-expr expr egg-graph (*context*)))
      (check-equal? (egg-expr->expr egg-expr egg-graph (context-repr (*context*))) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Proofs
;;
;; Proofs from egg contain let expressions (not Scheme like) as
;; well as other information about rewrites; proof extraction requires
;; some flattening and translation

(define (remove-rewrites proof)
  (match proof
    [`(Rewrite=> ,_ ,something) (remove-rewrites something)]
    [`(Rewrite<= ,_ ,something) (remove-rewrites something)]
    [(list _ ...) (map remove-rewrites proof)]
    [_ proof]))

;; Performs a product, but traverses the elements in order
;; This is the core logic of flattening a proof given flattened proofs for each child of a node
(define (sequential-product elements)
  (cond
    [(empty? elements) (list empty)]
    [else
     (define without-rewrites (remove-rewrites (last (first elements))))
     (append (for/list ([head (first elements)])
               (cons head (map first (rest elements))))
             (for/list ([other (in-list (rest (sequential-product (rest elements))))])
               (cons without-rewrites other)))]))

;; returns a flattened list of terms
;; The first term has no rewrite- the rest have exactly one rewrite
(define (expand-proof-term term budget)
  (let loop ([term term])
    (cond
      [(<= (unbox budget) 0) (list #f)]
      [else
       (match term
         [(? symbol?) (list term)]
         [(? literal?) (list term)]
         [(? number?) (list term)]
         [(approx spec impl)
          (define children (list (loop spec) (loop impl)))
          (cond
            [(member (list #f) children) (list #f)]
            [else
             (define res (sequential-product children))
             (set-box! budget (- (unbox budget) (length res)))
             (map (curry apply approx) res)])]
         [`(Explanation ,body ...) (expand-proof body budget)]
         [(? list?)
          (define children (map loop term))
          (cond
            [(member (list #f) children) (list #f)]
            [else
             (define res (sequential-product children))
             (set-box! budget (- (unbox budget) (length res)))
             res])]
         [_ (error "Unknown proof term ~a" term)])])))

;; Remove the front term if it doesn't have any rewrites
(define (remove-front-term proof)
  (if (equal? (remove-rewrites (first proof)) (first proof))
      (rest proof)
      proof))

;; converts a let-bound tree explanation
;; into a flattened proof for use by Herbie
(define (expand-proof proof budget)
  (define expanded (map (curryr expand-proof-term budget) proof))
  ;; get rid of any unnecessary terms
  (define contiguous (cons (first expanded) (map remove-front-term (rest expanded))))
  ;; append together the proofs
  (define res (apply append contiguous))
  (set-box! budget (- (unbox budget) (length proof)))
  (if (member #f res)
      (list #f)
      res))

(module+ test
  (check-equal? (sequential-product `((1 2) (3 4 5) (6))) `((1 3 6) (2 3 6) (2 4 6) (2 5 6)))

  (check-equal? (expand-proof-term '(Explanation (+ x y) (+ y x)) (box 10)) '((+ x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule expansion
;;
;; Expansive rules are the only problematic rules.
;; We only support expansive rules where the LHS is a spec.

;; Translates a Herbie rule into an egg rule
(define (rule->egg-rule ru)
  (struct-copy rule
               ru
               [input (expr->egg-pattern (rule-input ru))]
               [output (expr->egg-pattern (rule-output ru))]))

(define (rule->egg-rules ru)
  (define input (rule-input ru))
  (cond
    [(symbol? input)
     ; expansive rules
     (define itype (dict-ref (rule-itypes ru) input))
     (for/list ([op (all-operators)]
                #:when (eq? (operator-info op 'otype) itype))
       (define itypes (operator-info op 'itype))
       (define vars (map (lambda (_) (gensym)) itypes))
       (rule (sym-append (rule-name ru) '-expand- op)
             (cons op vars)
             (replace-expression (rule-output ru) input (cons op vars))
             (map cons vars itypes)
             (rule-otype ru)
             (rule-tags ru)))]
    ; non-expansive rule
    [else (list (rule->egg-rule ru))]))

;; egg rule cache: rule -> (cons/c rule FFI-rule)
(define/reset *egg-rule-cache* (make-hasheq))

;; Cache mapping (expanded) rule name to its canonical rule name
(define/reset *canon-names* (make-hasheq))

;; Tries to look up the canonical name of a rule using the cache.
;; Obviously dangerous if the cache is invalid.
(define (get-canon-rule-name name [failure #f])
  (hash-ref (*canon-names*) name failure))

;; Expand and convert the rules for egg.
;; Uses a cache to only expand each rule once.
(define (expand-rules rules)
  (reap [sow]
        (for ([rule (in-list rules)])
          (define egg&ffi-rules
            (hash-ref! (*egg-rule-cache*)
                       rule
                       (lambda ()
                         (for/list ([egg-rule (in-list (rule->egg-rules rule))])
                           (define name (rule-name egg-rule))
                           (define ffi-rule
                             (make-ffi-rule name (rule-input egg-rule) (rule-output egg-rule)))
                           (hash-set! (*canon-names*) name (rule-name rule))
                           (cons egg-rule ffi-rule)))))
          (for-each sow egg&ffi-rules))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket egraph
;;
;; Racket representation of a typed egraph.
;; Given an e-graph from egg-herbie, we can split every e-class
;; by type ensuring that every term in an e-class has the same output type.
;; This trick makes extraction easier.

;; - eclasses: vector of enodes
;; - types: vector-map from e-class to type/representation
;; - leaf?: vector-map from e-class to boolean indicating if it contains a leaf node
;; - constants: vector-map from e-class to a number or #f
;; - specs: vector-map from e-class to an approx spec or #f
;; - parents: vector-map from e-class to its parent e-classes (as a vector)
;; - canon: map from (Rust) e-class, type to (Racket) e-class
;; - egg->herbie: data to translate egg IR to herbie IR
(struct regraph (eclasses types leaf? constants specs parents canon egg->herbie))

;; Returns all representatations (and their types) in the current platform.
(define (all-reprs/types [pform (*active-platform*)])
  (remove-duplicates (append-map (lambda (repr) (list repr (representation-type repr)))
                                 (platform-reprs pform))))

;; Returns the type(s) of an enode so it can be placed in the proper e-class.
;; Typing rules:
;;  - numbers: every real representation (or real type)
;;  - variables: lookup in the `egg->herbie` renaming dictionary
;;  - `if`: type is every representation (or type) [can prune incorrect ones]
;;  - `approx`: every real representation [can prune incorrect ones]
;;  - ops/impls: its output type/representation
;; NOTE: we can constrain "every" type by using the platform.
(define (enode-type enode egg->herbie)
  (match enode
    [(? number?) (cons 'real (platform-reprs (*active-platform*)))] ; number
    [(? symbol?) ; variable
     (match-define (cons _ repr) (hash-ref egg->herbie enode (lambda () (hash-ref egg->herbie '$h0))))
     (list repr (representation-type repr))]
    [(cons f _) ; application
     (cond
       [(eq? f '$approx) (platform-reprs (*active-platform*))]
       [(eq? f 'if) (all-reprs/types)]
       [(impl-exists? f) (list (impl-info f 'otype))]
       [else (list (operator-info f 'otype))])]))

;; Rebuilds an e-node using typed e-classes
(define (rebuild-enode enode type lookup)
  (match enode
    [(? number?) enode] ; number
    [(? symbol?) enode] ; variable
    [(cons f ids) ; application
     (cond
       [(eq? f '$approx) ; approx node
        (define spec (u32vector-ref ids 0))
        (define impl (u32vector-ref ids 1))
        (list '$approx (lookup spec (representation-type type)) (lookup impl type))]
       [(eq? f 'if) ; if expression
        (define cond (u32vector-ref ids 0))
        (define ift (u32vector-ref ids 1))
        (define iff (u32vector-ref ids 2))
        (define cond-type
          (if (representation? type)
              (get-representation 'bool)
              'bool))
        (list 'if (lookup cond cond-type) (lookup ift type) (lookup iff type))]
       [else
        (define itypes
          (if (impl-exists? f)
              (impl-info f 'itype)
              (operator-info f 'itype)))
        ; unsafe since we don't check that |itypes| = |ids|
        ; optimize for common cases to avoid extra allocations
        (cons
         f
         (match itypes
           [(list) '()]
           [(list t1) (list (lookup (u32vector-ref ids 0) t1))]
           [(list t1 t2) (list (lookup (u32vector-ref ids 0) t1) (lookup (u32vector-ref ids 1) t2))]
           [(list t1 t2 t3)
            (list (lookup (u32vector-ref ids 0) t1)
                  (lookup (u32vector-ref ids 1) t2)
                  (lookup (u32vector-ref ids 2) t3))]
           [_ (map lookup (u32vector->list ids) itypes)]))])]))

;; Splits untyped eclasses into typed eclasses.
;; Nodes are duplicated across their possible types.
(define (split-untyped-eclasses egraph-data egg->herbie)
  (define eclass-ids (egraph-eclasses egraph-data))
  (define max-id
    (for/fold ([current-max 0]) ([egg-id (in-u32vector eclass-ids)])
      (max current-max egg-id)))
  (define egg-id->idx (make-u32vector (+ max-id 1)))
  (for ([egg-id (in-u32vector eclass-ids)]
        [idx (in-naturals)])
    (u32vector-set! egg-id->idx egg-id idx))

  (define types (all-reprs/types))
  (define type->idx (make-hasheq))
  (for ([type (in-list types)]
        [idx (in-naturals)])
    (hash-set! type->idx type idx))
  (define num-types (hash-count type->idx))

  ; maps (idx, type) to type eclass id
  (define (idx+type->id idx type)
    (+ (* idx num-types) (hash-ref type->idx type)))

  ; maps (untyped eclass id, type) to typed eclass id
  (define (lookup-id eid type)
    (idx+type->id (u32vector-ref egg-id->idx eid) type))

  ; allocate enough eclasses for every (egg-id, type) combination
  (define n (* (u32vector-length eclass-ids) num-types))
  (define id->eclass (make-vector n '()))
  (define id->parents (make-vector n '()))
  (define id->leaf? (make-vector n #f))

  ; for each eclass, extract the enodes
  ;  <enode> ::= <symbol>
  ;            | <number>
  ;            | (<symbol> . <u32vector>)
  ; NOTE: nodes in typed eclasses are reversed relative
  ; to their position in untyped eclasses
  (for ([eid (in-u32vector eclass-ids)]
        [idx (in-naturals)])
    (define enodes (egraph-get-eclass egraph-data eid))
    (for ([enode (in-vector enodes)])
      ; get all possible types for the enode
      ; lookup its correct eclass and add the rebuilt node
      (define types (enode-type enode egg->herbie))
      (for ([type (in-list types)])
        (define id (idx+type->id idx type))
        (define enode* (rebuild-enode enode type lookup-id))
        (vector-set! id->eclass id (cons enode* (vector-ref id->eclass id)))
        (match enode*
          [(list _ ids ...)
           (if (null? ids)
               (vector-set! id->leaf? id #t)
               (for ([child-id (in-list ids)])
                 (vector-set! id->parents child-id (cons id (vector-ref id->parents child-id)))))]
          [(? symbol?) (vector-set! id->leaf? id #t)]
          [(? number?) (vector-set! id->leaf? id #t)]))))

  ; dedup `id->parents` values
  (for ([id (in-range n)])
    (vector-set! id->parents id (list->vector (remove-duplicates (vector-ref id->parents id)))))
  (values id->eclass id->parents id->leaf? eclass-ids egg-id->idx type->idx))

;; TODO: reachable from roots?
;; Prunes e-nodes that are not well-typed.
;; An e-class is well-typed if it has one well-typed node
;; A node is well-typed if all of its child e-classes are well-typed.
(define (prune-ill-typed! id->eclass id->parents id->leaf?)
  (define n (vector-length id->eclass))

  ;; is the e-class well-typed?
  (define typed?-vec (make-vector n #f))
  (define (eclass-well-typed? id)
    (vector-ref typed?-vec id))

  ;; is the e-node well-typed?
  (define (enode-typed? enode)
    (or (number? enode) (symbol? enode) (and (list? enode) (andmap eclass-well-typed? (cdr enode)))))

  (define (check-typed! dirty?-vec)
    (define dirty? #f)
    (define dirty?-vec* (make-vector n #f))
    (for ([id (in-range n)]
          #:when (vector-ref dirty?-vec id))
      (unless (vector-ref typed?-vec id)
        (when (ormap enode-typed? (vector-ref id->eclass id))
          (vector-set! typed?-vec id #t)
          (define parent-ids (vector-ref id->parents id))
          (unless (vector-empty? parent-ids)
            (set! dirty? #t)
            (for ([parent-id (in-vector parent-ids)])
              (vector-set! dirty?-vec* parent-id #t))))))
    (when dirty?
      (check-typed! dirty?-vec*)))

  ; mark all well-typed e-classes and prune nodes that are not well-typed
  (check-typed! (vector-copy id->leaf?))
  (for ([id (in-range n)])
    (define eclass (vector-ref id->eclass id))
    (vector-set! id->eclass id (filter enode-typed? eclass)))

  ; sanity check: every child id points to a non-empty e-class
  (for ([id (in-range n)])
    (define eclass (vector-ref id->eclass id))
    (for ([enode (in-list eclass)])
      (match enode
        [(list _ ids ...)
         (for ([id (in-list ids)])
           (when (null? (vector-ref id->eclass id))
             (error 'prune-ill-typed!
                    "eclass ~a is empty, eclasses ~a"
                    id
                    (for/vector #:length n
                                ([id (in-range n)])
                      (list id (vector-ref id->eclass id))))))]
        [_ (void)]))))

;; Rebuilds eclasses and associated data after pruning.
(define (rebuild-eclasses id->eclass eclass-ids egg-id->idx type->idx)
  (define n (vector-length id->eclass))
  (define remap (make-vector n #f))

  ; build the id map
  (define n* 0)
  (for ([id (in-range n)])
    (define eclass (vector-ref id->eclass id))
    (unless (null? eclass)
      (vector-set! remap id n*)
      (set! n* (add1 n*))))

  ; invert `type->idx` map
  (define idx->type (make-hash))
  (define num-types (hash-count type->idx))
  (for ([(type idx) (in-hash type->idx)])
    (hash-set! idx->type idx type))

  ; rebuild eclass and type vectors
  ; transform each eclass from a list to a vector
  (define eclasses (make-vector n* #f))
  (define types (make-vector n* #f))
  (for ([id (in-range n)])
    (define id* (vector-ref remap id))
    (when id*
      (define eclass (vector-ref id->eclass id))
      (vector-set! eclasses
                   id*
                   (for/vector #:length (length eclass)
                               ([enode (in-list eclass)])
                     (match enode
                       [(? number?) enode]
                       [(? symbol?) enode]
                       [(list op ids ...)
                        (define ids* (map (lambda (id) (vector-ref remap id)) ids))
                        (cons op ids*)])))
      (vector-set! types id* (hash-ref idx->type (modulo id num-types)))))

  ; build the canonical id map
  (define egg-id->id (make-hash))
  (for ([eid (in-u32vector eclass-ids)])
    (define idx (u32vector-ref egg-id->idx eid))
    (define id0 (* idx num-types))
    (for ([id (in-range id0 (+ id0 num-types))])
      (define id* (vector-ref remap id))
      (when id*
        (define type (vector-ref types id*))
        (hash-set! egg-id->id (cons eid type) id*))))

  (values eclasses types egg-id->id))

;; Splits untyped eclasses into typed eclasses,
;; keeping only the subset of enodes that are well-typed.
(define (make-typed-eclasses egraph-data egg->herbie)
  ;; Step 1: split Rust-eclasses by type
  (define-values (id->eclass id->parents id->leaf? eclass-ids egg-id->idx type->idx)
    (split-untyped-eclasses egraph-data egg->herbie))

  ;; Step 2: keep well-typed e-nodes
  ;; An e-class is well-typed if it has one well-typed node
  ;; A node is well-typed if all of its child e-classes are well-typed.
  (prune-ill-typed! id->eclass id->parents id->leaf?)

  ;; Step 3: remap e-classes
  ;; Any empty e-classes must be removed, so we re-map every id
  (rebuild-eclasses id->eclass eclass-ids egg-id->idx type->idx))

;; Analyzes eclasses for their properties.
;; The result are vector-maps from e-class ids to data.
;;  - parents: parent e-classes (as a vector)
;;  - leaf?: does the e-class contain a leaf node
;;  - constants: the e-class constant (if one exists)
(define (analyze-eclasses eclasses)
  (define n (vector-length eclasses))
  (define parents (make-vector n '()))
  (define leaf? (make-vector n '#f))
  (define constants (make-vector n #f))
  (for ([id (in-range n)])
    (define eclass (vector-ref eclasses id))
    (for ([enode eclass]) ; might be a list or vector
      (match enode
        [(? number? n)
         (vector-set! leaf? id #t)
         (vector-set! constants id n)]
        [(? symbol?) (vector-set! leaf? id #t)]
        [(list _ ids ...)
         (when (null? ids)
           (vector-set! leaf? id #t))
         (for ([child-id (in-list ids)])
           (vector-set! parents child-id (cons id (vector-ref parents child-id))))])))

  ; parent map: remove duplicates, convert lists to vectors
  (for ([id (in-range n)])
    (define ids (remove-duplicates (vector-ref parents id)))
    (vector-set! parents id (list->vector ids)))

  (values parents leaf? constants))

;; Constructs a Racket egraph from an S-expr representation of
;; an egraph and data to translate egg IR to herbie IR.
(define (make-regraph egraph-data)
  (define timeline-end! (timeline-start! 'times (~a "i-make-regraph")))
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (define id->spec (egraph-data-id->spec egraph-data))

  ;; split the e-classes by type
  (define-values (eclasses types canon) (make-typed-eclasses egraph-data egg->herbie))
  (define n (vector-length eclasses))
  (eprintf "#eclasses from make-regraph: ~a\n" n)

  ;; analyze each eclass
  (define-values (parents leaf? constants) (analyze-eclasses eclasses))

  ;; rebuild id->spec map for typed e-classes
  (define specs (make-vector n #f))
  (for ([(id spec&repr) (in-hash id->spec)])
    (match-define (cons spec repr) spec&repr)
    (define id* (hash-ref canon (cons (egraph-find egraph-data id) repr)))
    (vector-set! specs id* spec))

  ; construct the `regraph` instance
  (define out (regraph eclasses types leaf? constants specs parents canon egg->herbie))
  (timeline-end!)
  out)

(define (regraph-nodes->json regraph)
  (define cost (platform-node-cost-proc (*active-platform*)))
  (for/hash ([n (in-naturals)]
             [eclass (in-vector (regraph-eclasses regraph))]
             #:when true
             [k (in-naturals)]
             [enode eclass])
    (define type (vector-ref (regraph-types regraph) n))
    (define cost
      (if (representation? type)
          (match enode
            [(? number?) (platform-repr-cost (*active-platform*) type)]
            [(? symbol?) (platform-repr-cost (*active-platform*) type)]
            [(list '$approx x y) 0]
            [(list 'if c x y)
             (match (platform-impl-cost (*active-platform*) 'if)
               [`(max ,n) n] ; Not quite right
               [`(sum ,n) n])]
            [(list op args ...) (platform-impl-cost (*active-platform*) op)])
          1))
    (values (string->symbol (format "~a.~a" n k))
            (hash 'op
                  (~a (if (list? enode)
                          (car enode)
                          enode))
                  'children
                  (if (list? enode)
                      (map (lambda (e) (format "~a.0" e)) (cdr enode))
                      '())
                  'eclass
                  (~a n)
                  'cost
                  cost))))

;; Egraph node has children.
;; Nullary operators have no children!
(define (node-has-children? node)
  (and (pair? node) (pair? (cdr node))))

;; Computes an analysis for each eclass.
;; Takes a regraph and an procedure taking the analysis, an eclass, and
;; its eclass id producing a non-`#f` result when the parents of the eclass
;; need to be revisited. Result is a vector where each entry is
;; the eclass's analysis.
(define (regraph-analyze regraph eclass-proc #:analysis [analysis #f])
  (define eclasses (regraph-eclasses regraph))
  (define leaf? (regraph-leaf? regraph))
  (define parents (regraph-parents regraph))
  (define n (vector-length eclasses))

  ; set analysis if not provided
  (unless analysis
    (set! analysis (make-vector n #f)))
  (define dirty?-vec (vector-copy leaf?)) ; visit eclass on next pass?
  (define changed?-vec (make-vector n #f)) ; eclass was changed last iteration

  ; run the analysis
  (let sweep! ([iter 0])
    (define dirty? #f)
    (define dirty?-vec* (make-vector n #f))
    (define changed?-vec* (make-vector n #f))
    (for ([id (in-range n)]
          #:when (vector-ref dirty?-vec id))
      (define eclass (vector-ref eclasses id))
      (when (eclass-proc analysis changed?-vec iter eclass id)
        ; eclass analysis was updated: need to revisit the parents
        (define parent-ids (vector-ref parents id))
        (vector-set! changed?-vec* id #t)
        (for ([parent-id (in-vector parent-ids)])
          (vector-set! dirty?-vec* parent-id #t)
          (set! dirty? #t))))
    ; if dirty, analysis has not converged so loop
    (when dirty?
      (set! dirty?-vec dirty?-vec*) ; update eclasses that require visiting
      (set! changed?-vec changed?-vec*) ; update eclasses that have changed
      (sweep! (add1 iter))))

  ; Invariant: all eclasses have an analysis
  (for ([id (in-range n)])
    (unless (vector-ref analysis id)
      (define types (regraph-types regraph))
      (error 'regraph-analyze
             "analysis not run on all eclasses: ~a ~a"
             eclass-proc
             (for/vector #:length n
                         ([id (in-range n)])
               (define type (vector-ref types id))
               (define eclass (vector-ref eclasses id))
               (define eclass-analysis (vector-ref analysis id))
               (list id type eclass eclass-analysis)))))

  analysis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regraph typed extraction
;;
;; Typed extraction is ideal for extracting specifications from an egraph.
;; By "typed", we refer to the output "representation" of a given operator.
;; This style of extractor associates to each eclass the best
;; (cost, node) pair for each possible output type in the eclass.
;; The extractor procedure takes an eclass id and an output type.
;;
;; Typed cost functions take:
;;  - the regraph we are extracting from
;;  - a mutable cache (to possibly stash per-node data)
;;  - the node we are computing cost for
;;  - 3 argument procedure taking:
;;       - an eclass id
;;       - an output type
;;       - a default failure value
;;

;; The typed extraction algorithm.
;; Extraction is partial, that is, the result of the extraction
;; procedure is `#f` if extraction finds no well-typed program
;; at a particular id with a particular output type.
(define ((typed-egg-batch-extractor cost-proc batch-extract-to) regraph)
  (define eclasses (regraph-eclasses regraph))
  (define types (regraph-types regraph))
  (define n (vector-length eclasses))

  ; e-class costs
  (define costs (make-vector n #f))

  ; looks up the cost
  (define (unsafe-eclass-cost id)
    (car (vector-ref costs id)))

  ; do its children e-classes have a cost
  (define (node-ready? node)
    (match node
      [(? number?) #t]
      [(? symbol?) #t]
      [(list '$approx _ impl) (vector-ref costs impl)]
      [(list _ ids ...) (andmap (lambda (id) (vector-ref costs id)) ids)]))

  ; computes cost of a node (as long as each of its children have costs)
  ; cost function has access to a mutable value through `cache`
  (define cache (box #f))
  (define (node-cost node type)
    (and (node-ready? node) (cost-proc regraph cache node type unsafe-eclass-cost)))

  ; updates the cost of the current eclass.
  ; returns whether the cost of the current eclass has improved.
  (define (eclass-set-cost! _ changed?-vec iter eclass id)
    (define type (vector-ref types id))
    (define updated? #f)

    ; update cost information
    (define (update-cost! new-cost node)
      (when new-cost
        (define prev-cost&node (vector-ref costs id))
        (when (or (not prev-cost&node) ; first cost
                  (< new-cost (car prev-cost&node))) ; better cost
          (vector-set! costs id (cons new-cost node))
          (set! updated? #t))))

    ; optimization: we only need to update node cost as needed.
    ;  (i) terminals, nullary operators: only compute once
    ;  (ii) non-nullary operators: compute when any of its child eclasses
    ;       have their analysis updated
    (define (node-requires-update? node)

      (if (node-has-children? node)
          (ormap (lambda (id) (vector-ref changed?-vec id)) (cdr node))
          (= iter 0)))

    ; iterate over each node
    (for ([node (in-vector eclass)])
      (when (node-requires-update? node)
        (define new-cost (node-cost node type))
        (update-cost! new-cost node)))

    updated?)

  ; run the analysis
  (regraph-analyze regraph eclass-set-cost! #:analysis costs)

  (define id->spec (regraph-specs regraph))

  (define egg->herbie (regraph-egg->herbie regraph))
  (define-values (add-id add-enode finalize-batch)
    (egg-nodes->batch costs id->spec batch-extract-to egg->herbie))
  ;; These functions provide a setup to extract nodes into batch-extract-to from nodes
  (list add-id add-enode finalize-batch))

;; Is fractional with odd denominator.
(define (fraction-with-odd-denominator? frac)
  (and (rational? frac) (let ([denom (denominator frac)]) (and (> denom 1) (odd? denom)))))

;; Decompose an e-node representing an impl of `(pow b e)`.
;; Returns either `#f` or the `(cons b e)`
(define (pow-impl-args impl args)
  (define vars (impl-info impl 'vars))
  (match (impl-info impl 'spec)
    [(list 'pow b e)
     #:when (set-member? vars e)
     (define env (map cons vars args))
     (define b* (dict-ref env b b))
     (define e* (dict-ref env e e))
     (cons b* e*)]
    [_ #f]))

;; Old cost model version
(define (default-egg-cost-proc regraph cache node type rec)
  (match node
    [(? number?) 1]
    [(? symbol?) 1]
    ; approx node
    [(list '$approx _ impl) (rec impl)]
    [(list 'if cond ift iff) (+ 1 (rec cond) (rec ift) (rec iff))]
    [(list (? impl-exists? impl) args ...)
     (match (pow-impl-args impl args)
       [(cons _ e)
        #:when (let ([n (vector-ref (regraph-constants regraph) e)])
                 (fraction-with-odd-denominator? n))
        +inf.0]
       [_ (apply + 1 (map rec args))])]
    [(list 'pow b e)
     (define n (vector-ref (regraph-constants regraph) e))
     (if (fraction-with-odd-denominator? n)
         +inf.0
         (+ 1 (rec b) (rec e)))]
    [(list _ args ...) (apply + 1 (map rec args))]))

;; Per-node cost function according to the platform
;; `rec` takes an id, type, and failure value
(define (platform-egg-cost-proc regraph cache node type rec)
  (cond
    [(representation? type)
     (define egg->herbie (regraph-egg->herbie regraph))
     (define node-cost-proc (platform-node-cost-proc (*active-platform*)))
     (match node
       ; numbers (repr is unused)
       [(? number? n) ((node-cost-proc (literal n type) type))]
       [(? symbol?) ; variables (`egg->herbie` has the repr)
        (define repr (cdr (hash-ref egg->herbie node)))
        ((node-cost-proc node repr))]
       ; approx node
       [(list '$approx _ impl) (rec impl)]
       [(list 'if cond ift iff) ; if expression
        (define cost-proc (node-cost-proc node type))
        (cost-proc (rec cond) (rec ift) (rec iff))]
       [(list (? impl-exists?) args ...) ; impls
        (define cost-proc (node-cost-proc node type))
        (apply cost-proc (map rec args))])]
    [else (default-egg-cost-proc regraph cache node type rec)]))

;; Extracts the best expression according to the extractor.
;; Result is a single element list.
(define (regraph-extract-best regraph extract id type)
  (define egg->herbie (regraph-egg->herbie regraph))
  (define canon (regraph-canon regraph))
  ; Extract functions to extract exprs from egraph
  (match-define (list extract-id _ _) extract)
  ; extract expr
  (define key (cons id type))
  (cond
    ; at least one extractable expression
    [(hash-has-key? canon key)
     (define id* (hash-ref canon key))
     (list (extract-id id* type))]
    ; no extractable expressions
    [else (list)]))

;; Extracts multiple expressions according to the extractor
(define (regraph-extract-variants regraph extract id type)
  ; regraph fields
  (define eclasses (regraph-eclasses regraph))
  (define id->spec (regraph-specs regraph))
  (define canon (regraph-canon regraph))
  ; Functions for egg-extraction
  (match-define (list _ extract-enode _) extract)
  ; extract expressions
  (define key (cons id type))
  (cond
    ; at least one extractable expression
    [(hash-has-key? canon key)
     (define id* (hash-ref canon key))

     (remove-duplicates (for/list ([enode (vector-ref eclasses id*)])
                          (extract-enode enode type))
                        #:key batchref-idx)]
    [else (list)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler
;;
;; A mini-interpreter for egraph "schedules" including running egg,
;; pruning certain kinds of nodes, extracting expressions, etc.

;; Runs rules over the egraph with the given egg parameters.
;; Invariant: the returned egraph is never unsound
(define (egraph-run-rules egg-graph0 egg-rules params)
  (define node-limit (dict-ref params 'node #f))
  (define iter-limit (dict-ref params 'iteration #f))
  (define scheduler (dict-ref params 'scheduler 'backoff))
  (define const-folding? (dict-ref params 'const-fold? #t))
  (define ffi-rules (map cdr egg-rules))

  ;; run the rules
  (let loop ([iter-limit iter-limit])
    (define egg-graph (egraph-copy egg-graph0))
    (define iteration-data
      (egraph-run egg-graph ffi-rules node-limit iter-limit scheduler const-folding?))

    (timeline-push! 'stop (egraph-stop-reason egg-graph) 1)
    (cond
      [(egraph-is-unsound-detected egg-graph)
       ; unsoundness means run again with less iterations
       (define num-iters (length iteration-data))
       (if (<= num-iters 1) ; nothing to fall back on
           (values egg-graph0 (list))
           (loop (sub1 num-iters)))]
      [else (values egg-graph iteration-data)])))

(define (egraph-run-schedule mode batch roots schedule ctx)
  ; allocate the e-graph
  (define egg-graph (make-egraph mode))

  ; insert expressions into the e-graph
  (define root-ids (egraph-add-exprs egg-graph batch roots ctx))

  ; run the schedule
  (define egg-graph*
    (for/fold ([egg-graph egg-graph]) ([(rules params) (in-dict schedule)])
      ; run rules in the egraph
      (define egg-rules (expand-rules rules))
      (define timeline-end! (timeline-start! 'times (~a "i-egraph-run-rules")))
      (define-values (egg-graph* iteration-data) (egraph-run-rules egg-graph egg-rules params))
      (timeline-end!)

      ; get cost statistics
      ;; (for ([iter (in-list iteration-data)]
      ;;       [i (in-naturals)])
      ;;   (define cnt (iteration-data-num-nodes iter))
      ;;   (define cost (apply + (map (λ (id) (egraph-get-cost egg-graph* id i)) root-ids)))
      ;;   (timeline-push! 'egraph i cnt cost (iteration-data-time iter)))

      egg-graph*))

  ; root eclasses may have changed
  (define root-ids* (map (lambda (id) (egraph-find egg-graph* id)) root-ids))
  ; return what we need
  (values root-ids* egg-graph*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;
;; Most calls to egg should be done through this interface.
;;  - `make-egg-runner`: creates a struct that describes a _reproducible_ egg instance
;;  - `run-egg`: takes an egg runner and performs an extraction (exprs or proof)

;; Herbie's version of an egg runner.
;; Defines parameters for running rewrite rules with egg
(struct egg-runner (batch roots reprs schedule ctx)
  #:transparent ; for equality
  #:methods gen:custom-write ; for abbreviated printing
  [(define (write-proc alt port mode)
     (fprintf port "#<egg-runner>"))])

;; Constructs an egg runner.
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
(define (make-egg-runner batch roots reprs schedule #:context [ctx (*context*)])
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
  (egg-runner batch roots reprs schedule ctx))

(define (regraph-dump regraph root-ids reprs)
  (define dump-dir "dump-egg")
  (unless (directory-exists? dump-dir)
    (make-directory dump-dir))
  (define name
    (for/first ([i (in-naturals)]
                #:unless (file-exists? (build-path dump-dir (format "~a.json" i))))
      (build-path dump-dir (format "~a.json" i))))
  (define nodes (regraph-nodes->json regraph))
  (define canon (regraph-canon regraph))
  (define roots
    (filter values
            (for/list ([id (in-list root-ids)]
                       [type (in-list reprs)])
              (hash-ref canon (cons id type) #f))))
  (call-with-output-file
   name
   #:exists 'replace
   (lambda (p) (write-json (hash 'nodes nodes 'root_eclasses (map ~a roots) 'class_data (hash)) p))))

;; Runs egg using an egg runner.
;;
;; Argument `cmd` specifies what to get from the e-graph:
;;  - single extraction: `(single . <extractor>)`
;;  - multi extraction: `(multi . <extractor>)`
;;  - proofs: `(proofs . ((<start> . <end>) ...))`
(define (run-egg runner cmd)
  ;; Run egg using runner
  (define ctx (egg-runner-ctx runner))
  (define timeline-end! (timeline-start! 'times (~a "i-egraph-run-schedule")))
  (define-values (root-ids egg-graph)
    (egraph-run-schedule (car cmd)
                         (egg-runner-batch runner)
                         (egg-runner-roots runner)
                         (egg-runner-schedule runner)
                         ctx))
  (timeline-end!)
  ; Perform extraction
  (define timeline-end-2! (timeline-start! 'times (~a "i-run-egg-extraction")))
  (define out (match cmd
    [`(single . ,extractor) ; single expression extraction
     (define regraph (make-regraph egg-graph))
     (define reprs (egg-runner-reprs runner))
     (when (flag-set? 'dump 'egg)
       (regraph-dump regraph root-ids reprs))

     (define extract-id (extractor regraph))
     (define finalize-batch (last extract-id))

     ; (Listof (Listof batchref))
     (define out
       (for/list ([id (in-list root-ids)]
                  [repr (in-list reprs)])
         (regraph-extract-best regraph extract-id id repr)))
     ; commit changes to the batch
     (finalize-batch)
     out]
    [`(multi . ,extractor) ; multi expression extraction
     (define regraph (make-regraph egg-graph))
     (define reprs (egg-runner-reprs runner))
     (when (flag-set? 'dump 'egg)
       (regraph-dump regraph root-ids reprs))

     (define extract-id (extractor regraph))
     (define finalize-batch (last extract-id))

     ; (Listof (Listof batchref))
     (define out
       (for/list ([id (in-list root-ids)]
                  [repr (in-list reprs)])
         (regraph-extract-variants regraph extract-id id repr)))
     ; commit changes to the batch
     (finalize-batch)
     out]
    [`(proofs . ((,start-exprs . ,end-exprs) ...)) ; proof extraction
     (for/list ([start (in-list start-exprs)]
                [end (in-list end-exprs)])
       (unless (egraph-expr-equal? egg-graph start end ctx)
         (error 'run-egg
                "cannot find proof; start and end are not equal.\n start: ~a \n end: ~a"
                start
                end))
       (define proof (egraph-get-proof egg-graph start end ctx))
       (when (null? proof)
         (error 'run-egg "proof extraction failed between`~a` and `~a`" start end))
       proof)]
    [`(equal? . ((,start-exprs . ,end-exprs) ...)) ; term equality?
     (for/list ([start (in-list start-exprs)]
                [end (in-list end-exprs)])
       (egraph-expr-equal? egg-graph start end ctx))]
    [_ (error 'run-egg "unknown command `~a`\n" cmd)]))
    (timeline-end-2!)
    out)
