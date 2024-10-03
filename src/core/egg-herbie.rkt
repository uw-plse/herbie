#lang racket

(require egg-herbie
         (only-in ffi/vector make-u32vector u32vector-set! list->u32vector))

(require "programs.rkt"
         "rules.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../config.rkt"
         "../utils/timeline.rkt"
         "rival.rkt"
         "../syntax/sugar.rkt"
         "points.rkt")

(provide (struct-out egg-runner)
         typed-egg-extractor
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
(define (make-egraph)
  (egraph-data (egraph_create) (make-hash) (make-hash) (make-hash)))

; Creates a new runner using an existing egraph.
; Useful for multi-phased rule application
(define (egraph-copy eg-data)
  (struct-copy egraph-data
               eg-data
               [egraph-pointer (egraph_copy (egraph-data-egraph-pointer eg-data))]))

; Adds expressions returning the root ids
; TODO: take a batch rather than list of expressions
(define (egraph-add-exprs egg-data exprs ctx)
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

  ; expr -> id
  ; expression cache
  (define expr->id (make-hash))

  ; expr -> natural
  ; inserts an expresison into the e-graph, returning its e-class id.
  (define (insert! expr [root? #f])
    ; transform the expression into a node pointing
    ; to its child e-classes
    (define node
      (match expr
        [(? number?) expr]
        [(? symbol?) (normalize-var expr)]
        [(literal v _) v]
        [(approx spec impl)
         (define spec* (insert! spec))
         (define impl* (insert! impl))
         (hash-ref! id->spec
                    spec*
                    (lambda ()
                      (define spec* (normalize-spec spec)) ; preserved spec for extraction
                      (define type (representation-type (repr-of impl ctx))) ; track type of spec
                      (cons spec* type)))
         (list '$approx spec* impl*)]
        [(list op args ...) (cons op (map insert! args))]))
    ; always insert the node if it is a root since
    ; the e-graph tracks which nodes are roots
    (cond
      [root? (insert-node! node #t)]
      [else (hash-ref! expr->id node (lambda () (insert-node! node #f)))]))

  (for/list ([expr (in-list exprs)])
    (insert! expr #t)))

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
  (egraph_run (egraph-data-egraph-pointer egraph-data)
              ffi-rules
              iter_limit
              node_limit
              simple_scheduler?
              const-folding?))

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

;; An egraph is just a S-expr of the form
;;  egraph ::= (<eclass> ...)
;;  eclass ::= (<id> <enode> ..+)
;;  enode  ::= (<op> <id> ...)
(define (egraph-serialize egraph-data)
  (egraph_serialize (egraph-data-egraph-pointer egraph-data)))

(define (egraph-find egraph-data id)
  (egraph_find (egraph-data-egraph-pointer egraph-data) id))

(define (egraph-expr-equal? egraph-data expr goal ctx)
  (match-define (list id1 id2) (egraph-add-exprs egraph-data (list expr goal) ctx))
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
  (let loop ([expr expr] [env (hash)])
    (match expr
      [(? number?) expr]
      [(? symbol?) (hash-ref env expr expr)]
      [`(let (,var ,term) ,body) (loop body (hash-set env var (loop term env)))]
      [`(,op ,args ...) (cons op (map (curryr loop env) args))])))

;; Converts an S-expr from egg into one Herbie understands
;; TODO: typing information is confusing since proofs mean
;; we may process mixed spec/impl expressions;
;; only need `type` to correctly interpret numbers
(define (egg-parsed->expr expr rename-dict type)
  (let loop ([expr expr] [type type])
    (match expr
      [(? number?) (if (representation? type) (literal expr (representation-name type)) expr)]
      [(? symbol?)
       (if (hash-has-key? rename-dict expr)
           (car (hash-ref rename-dict expr)) ; variable (extract uncanonical name)
           (list expr))] ; constant function
      [(list '$approx spec impl) ; approx
       (define spec-type (if (representation? type) (representation-type type) type))
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
  (if (equal? (remove-rewrites (first proof)) (first proof)) (rest proof) proof))

;; converts a let-bound tree explanation
;; into a flattened proof for use by Herbie
(define (expand-proof proof budget)
  (define expanded (map (curryr expand-proof-term budget) proof))
  ;; get rid of any unnecessary terms
  (define contiguous (cons (first expanded) (map remove-front-term (rest expanded))))
  ;; append together the proofs
  (define res (apply append contiguous))
  (set-box! budget (- (unbox budget) (length proof)))
  (if (member #f res) (list #f) res))

(module+ test
  (check-equal? (sequential-product `((1 2) (3 4 5) (6))) `((1 3 6) (2 3 6) (2 4 6) (2 5 6)))

  (expand-proof-term '(Explanation (+ x y) (+ y x)) (box 10)))

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
     (unless (type-name? itype)
       (error 'rule->egg-rules "expansive rules over impls is unsound ~a" input))
     (for/list ([op (all-operators)] #:when (eq? (operator-info op 'otype) itype))
       (define itypes (operator-info op 'itype))
       (define vars (map (lambda (_) (gensym)) itypes))
       (rule (sym-append (rule-name ru) '-expand- op)
             (cons op vars)
             (replace-expression (rule-output ru) input (cons op vars))
             (map cons vars itypes)
             (rule-otype ru)))]
    ; non-expansive rule
    [else (list (rule->egg-rule ru))]))

;; egg rule cache
(define/reset *egg-rule-cache* (make-hash))

;; Cache mapping name to its canonical rule name
;; See `*egg-rules*` for details
(define/reset *canon-names* (make-hash))

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
                       (cons (*active-platform*) rule)
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

;; Normalizes a Rust e-class.
;; Nullary operators are serialized as symbols, so we need to fix them.
(define (normalize-enode enode egg->herbie)
  (if (and (symbol? enode) (not (hash-has-key? egg->herbie enode))) (list enode) enode))

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
    [(? number?) (cons 'real (platform-reprs (*active-platform*)))]
    [(? symbol?)
     (match-define (cons _ repr) (hash-ref egg->herbie enode))
     (list repr (representation-type repr))]
    [(list '$approx _ _) (platform-reprs (*active-platform*))]
    [(list 'if _ _ _) (all-reprs/types)]
    [(list (? impl-exists? impl) _ ...) (list (impl-info impl 'otype))]
    [(list op _ ...) (list (operator-info op 'otype))]))

;; Rebuilds an e-node using typed e-classes
(define (rebuild-enode enode type lookup)
  (match enode
    [(? number?) enode]
    [(? symbol?) enode]
    [(list '$approx spec impl)
     (list '$approx (lookup spec (representation-type type)) (lookup impl type))]
    [(list 'if cond ift iff)
     (define cond-type (if (representation? type) (get-representation 'bool) 'bool))
     (list 'if (lookup cond cond-type) (lookup ift type) (lookup iff type))]
    [(list (? impl-exists? impl) args ...) (cons impl (map lookup args (impl-info impl 'itype)))]
    [(list op args ...) (cons op (map lookup args (operator-info op 'itype)))]))

;; Splits untyped eclasses into typed eclasses.
;; Nodes are duplicated across their possible types.
(define (split-untyped-eclasses egraph egg->herbie)
  ; optimization: use two dimensional table for lookups
  ; first indexed by untyped e-class id, then by type
  (define egg-id->table (make-hash)) ; natural -> (type -> id)
  (define egg-id->id (make-hash)) ; (natural, type) -> id [actual table in the result]
  (define id->eclass (make-hash)) ; natural -> box
  (define eclass-boxes '()) ; (listof box)
  (define eclass-types '()) ; (listof any)

  ;; Creates a fresh eclass for a given type.
  (define (new-eclass eid type)
    (define id (hash-count egg-id->id))
    (define eclass (box '()))
    (hash-set! egg-id->id (cons eid type) id)
    (hash-set! id->eclass id eclass)
    (set! eclass-boxes (cons eclass eclass-boxes))
    (set! eclass-types (cons type eclass-types))
    id)

  ;; Looks up the canonical id for a given egg id and type.
  ;; Returns a fresh id if none exists.
  (define (lookup-id eid type)
    (define type->id (hash-ref! egg-id->table eid (lambda () (make-hasheq))))
    (hash-ref! type->id type (lambda () (new-eclass eid type))))

  ;; build typed eclasses
  (for ([eclass (in-list egraph)])
    (define eid (car eclass))
    (for ([enode (in-list (cdr eclass))])
      (let ([enode (normalize-enode enode egg->herbie)])
        ; get all possible types for the enode
        (define types (enode-type enode egg->herbie))
        (for ([type (in-list types)])
          ; lookup or create a new typed eclass
          (define id (lookup-id eid type))
          (define enode* (rebuild-enode enode type lookup-id))
          (define eclass (hash-ref id->eclass id))
          (set-box! eclass (cons enode* (unbox eclass)))))))

  (define eclasses (list->vector (map (compose reverse unbox) (reverse eclass-boxes))))
  (define types (list->vector (reverse eclass-types)))
  (values eclasses types egg-id->id))

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

;; Prunes e-nodes that are not well-typed.
;; An e-class is well-typed if it has one well-typed node
;; A node is well-typed if all of its child e-classes are well-typed.
(define (prune-ill-typed! eclasses)
  (define n (vector-length eclasses))
  (define-values (parents leaf? _) (analyze-eclasses eclasses))

  ;; is the e-class well-typed
  (define typed? (make-vector n #f))

  (define (enode-typed? enode)
    (match enode
      [(? number?) #t]
      [(? symbol?) #t]
      [(list _ ids ...)
       (for/and ([id (in-list ids)])
         (vector-ref typed? id))]))

  (define (check-typed! dirty?-vec)
    (define dirty? #f)
    (define dirty?-vec* (make-vector n #f))
    (for ([id (in-range n)] #:when (vector-ref dirty?-vec id))
      (unless (vector-ref typed? id)
        (define eclass (vector-ref eclasses id))
        (when (ormap enode-typed? eclass)
          (vector-set! typed? id #t)
          (set! dirty? #t)
          (define parent-ids (vector-ref parents id))
          (for ([parent-id (in-vector parent-ids)])
            (vector-set! dirty?-vec* parent-id #t)))))
    (when dirty?
      (check-typed! dirty?-vec*)))

  ; mark all well-typed e-classes and prune nodes that are not well-typed
  (check-typed! (vector-copy leaf?))
  (for ([id (in-range n)])
    (define eclass (vector-ref eclasses id))
    (vector-set! eclasses id (filter enode-typed? eclass)))

  ; sanity check: every child id points to a non-empty e-class
  (for ([id (in-range n)])
    (define eclass (vector-ref eclasses id))
    (for ([enode (in-list eclass)])
      (match enode
        [(list _ ids ...)
         (for ([id (in-list ids)])
           (when (null? (vector-ref eclasses id))
             (error 'prune-ill-typed!
                    "eclass ~a is empty, eclasses ~a"
                    id
                    (for/vector #:length n
                                ([id (in-range n)])
                      (list id (vector-ref eclasses id))))))]
        [_ (void)]))))

;; Rebuilds eclasses and associated data after pruning.
(define (rebuild-eclasses eclasses types egg-id->id)
  (define n (vector-length eclasses))
  (define remap (make-vector n #f))

  (define n* 0)
  (for ([id (in-range n)])
    (define eclass (vector-ref eclasses id))
    (unless (null? eclass)
      (vector-set! remap id n*)
      (set! n* (add1 n*))))

  ; rebuild eclass vector
  ; transform each eclass from a list to a vector
  (define eclasses* (make-vector n* #f))
  (for ([id (in-range n)] #:when (vector-ref remap id))
    (define eclass (vector-ref eclasses id))
    (vector-set! eclasses*
                 (vector-ref remap id)
                 (for/vector #:length (length eclass)
                             ([enode (in-list eclass)])
                   (match enode
                     [(? number?) enode]
                     [(? symbol?) enode]
                     [(list op ids ...) (cons op (map (lambda (id) (vector-ref remap id)) ids))]))))

  ; rebuild the canonical id map
  (define egg-id->id* (make-hash))
  (for ([(k id) (in-hash egg-id->id)])
    (define id* (vector-ref remap id))
    (when id*
      (hash-set! egg-id->id* k id*)))

  ; rebuild the eclass type map
  (define types*
    (for/vector #:length n*
                ([id (in-range n)] #:when (vector-ref remap id))
      (vector-ref types id)))

  (values eclasses* types* egg-id->id*))

;; Splits untyped eclasses into typed eclasses,
;; keeping only the subset of enodes that are well-typed.
(define (make-typed-eclasses egraph egg->herbie)
  ;; Step 1: split Rust e-classes by type
  ;; The result are the eclasses, their types,
  ;; and a canonicalization map for egg e-class ids
  (define-values (eclasses types egg-id->id) (split-untyped-eclasses egraph egg->herbie))

  ;; Step 2: keep well-typed e-nodes
  ;; An e-class is well-typed if it has one well-typed node
  ;; A node is well-typed if all of its child e-classes are well-typed.
  (prune-ill-typed! eclasses)

  ;; Step 3: remap e-classes
  ;; Any empty e-classes must be removed, so we re-map every id
  (rebuild-eclasses eclasses types egg-id->id))

;; Constructs a Racket egraph from an S-expr representation of
;; an egraph and data to translate egg IR to herbie IR.
(define (make-regraph egraph-data)
  (define egg->herbie (egraph-data-egg->herbie-dict egraph-data))
  (define id->spec (egraph-data-id->spec egraph-data))

  ;; serialize the e-graph into Racket
  (define egraph (egraph-serialize egraph-data))

  ;; split the e-classes by type
  (define-values (eclasses types canon) (make-typed-eclasses egraph egg->herbie))
  (define n (vector-length eclasses))

  ;; analyze each eclass
  (define parents (make-vector n '()))
  (define leaf? (make-vector n '#f))
  (define constants (make-vector n #f))
  (for ([id (in-range n)])
    (define eclass (vector-ref eclasses id))
    (for ([enode (in-vector eclass)])
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

  ; convert id->spec to a vector-map
  (define specs (make-vector n #f))
  (for ([(id spec&repr) (in-hash id->spec)])
    (match-define (cons spec repr) spec&repr)
    (define id* (hash-ref canon (cons (egraph-find egraph-data id) repr)))
    (vector-set! specs id* spec))

  ; construct the `regraph` instance
  (regraph eclasses types leaf? constants specs parents canon egg->herbie))

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
    (for ([id (in-range n)] #:when (vector-ref dirty?-vec id))
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
(define ((typed-egg-extractor cost-proc) regraph)
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

  ; rebuilds the extracted procedure
  (define id->spec (regraph-specs regraph))
  (define (build-expr id)
    (let loop ([id id])
      (match (cdr (vector-ref costs id))
        [(? number? n) n] ; number
        [(? symbol? s) s] ; variable
        [(list '$approx spec impl) ; approx
         (match (vector-ref id->spec spec)
           [#f (error 'build-expr "no initial approx node in eclass ~a" id)]
           [spec-e (list '$approx spec-e (build-expr impl))])]
        ; if expression
        [(list 'if cond ift iff) (list 'if (loop cond) (loop ift) (loop iff))]
        ; expression of impls
        [(list (? impl-exists? impl) ids ...) (cons impl (map loop ids))]
        ; expression of operators
        [(list (? operator-exists? op) ids ...) (cons op (map loop ids))])))

  ; the actual extraction procedure
  ; as long as the `id` is valid, extraction will work
  (lambda (id) (cons (unsafe-eclass-cost id) (build-expr id))))

;; Is fractional with odd denominator.
(define (fraction-with-odd-denominator? frac)
  (and (rational? frac) (let ([denom (denominator frac)]) (and (> denom 1) (odd? denom)))))

;; Old cost model version
(define (default-egg-cost-proc regraph cache node type rec)
  (match node
    [(? number?) 1]
    [(? symbol?) 1]
    ; approx node
    [(list '$approx _ impl) (rec impl)]
    [(list 'if cond ift iff) (+ 1 (rec cond) (rec ift) (rec iff))]
    [(list (? impl-exists? impl) args ...)
     (cond
       [(equal? (impl->operator impl) 'pow)
        (match-define (list b e) args)
        (define n (vector-ref (regraph-constants regraph) e))
        (if (fraction-with-odd-denominator? n) +inf.0 (+ 1 (rec b) (rec e)))]
       [else (apply + 1 (map rec args))])]
    [(list 'pow b e)
     (define n (vector-ref (regraph-constants regraph) e))
     (if (fraction-with-odd-denominator? n) +inf.0 (+ 1 (rec b) (rec e)))]
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
  ; extract expr
  (define key (cons id type))
  (cond
    ; at least one extractable expression
    [(hash-has-key? canon key)
     (define id* (hash-ref canon key))
     (match-define (cons _ egg-expr) (extract id*))
     (list (egg-parsed->expr (flatten-let egg-expr) egg->herbie type))]
    ; no extractable expressions
    [else (list)]))

;; Extracts multiple expressions according to the extractor
(define (regraph-extract-variants regraph extract id type)
  ; regraph fields
  (define eclasses (regraph-eclasses regraph))
  (define id->spec (regraph-specs regraph))
  (define canon (regraph-canon regraph))

  ; extract expressions
  (define key (cons id type))
  (cond
    ; at least one extractable expression
    [(hash-has-key? canon key)
     (define id* (hash-ref canon key))
     (define egg-exprs
       (for/list ([enode (vector-ref eclasses id*)])
         (match enode
           [(? number?) enode]
           [(? symbol?) enode]
           [(list '$approx spec impl)
            (define spec* (vector-ref id->spec spec))
            (unless spec*
              (error 'regraph-extract-variants "no initial approx node in eclass ~a" id*))
            (match-define (cons _ impl*) (extract impl))
            (list '$approx spec* impl*)]
           [(list 'if cond ift iff)
            (match-define (cons _ cond*) (extract cond))
            (match-define (cons _ ift*) (extract ift))
            (match-define (cons _ iff*) (extract iff))
            (list 'if cond* ift* iff*)]
           [(list (? impl-exists? impl) ids ...)
            (define args
              (for/list ([id (in-list ids)])
                (match-define (cons _ expr) (extract id))
                expr))
            (cons impl args)]
           [(list (? operator-exists? op) ids ...)
            (define args
              (for/list ([id (in-list ids)])
                (match-define (cons _ expr) (extract id))
                expr))
            (cons op args)])))
     ; translate egg IR to Herbie IR
     (define egg->herbie (regraph-egg->herbie regraph))
     (for/list ([egg-expr (in-list egg-exprs)])
       (egg-parsed->expr (flatten-let egg-expr) egg->herbie type))]
    ; no extractable expressions
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

(define (egraph-run-schedule exprs schedule ctx)
  ; allocate the e-graph
  (define egg-graph (make-egraph))

  ; insert expressions into the e-graph
  (define root-ids (egraph-add-exprs egg-graph exprs ctx))

  ; run the schedule
  (define rule-apps (make-hash))
  (define egg-graph*
    (for/fold ([egg-graph egg-graph]) ([instr (in-list schedule)])
      (match-define (cons rules params) instr)
      ; run rules in the egraph
      (define egg-rules (expand-rules rules))
      (define-values (egg-graph* iteration-data) (egraph-run-rules egg-graph egg-rules params))

      ; get cost statistics
      (for/fold ([time 0]) ([iter (in-list iteration-data)] [i (in-naturals)])
        (define cnt (iteration-data-num-nodes iter))
        (define cost (apply + (map (λ (id) (egraph-get-cost egg-graph* id i)) root-ids)))
        (define new-time (+ time (iteration-data-time iter)))
        (timeline-push! 'egraph i cnt cost new-time)
        new-time)

      ;; get rule statistics
      (for ([(egg-rule ffi-rule) (in-dict egg-rules)])
        (define count (egraph-get-times-applied egg-graph* ffi-rule))
        (define canon-name (hash-ref (*canon-names*) (rule-name egg-rule)))
        (hash-update! rule-apps canon-name (curry + count) count))

      egg-graph*))

  ; report rule statistics
  (for ([(name count) (in-hash rule-apps)])
    (when (> count 0)
      (timeline-push! 'rules (~a name) count)))
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
(struct egg-runner (exprs reprs schedule ctx)
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
(define (make-egg-runner exprs reprs schedule #:context [ctx (*context*)])
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
  (egg-runner exprs reprs schedule ctx))

;; Runs egg using an egg runner.
;;
;; Argument `cmd` specifies what to get from the e-graph:
;;  - single extraction: `(single . <extractor>)`
;;  - multi extraction: `(multi . <extractor>)`
;;  - proofs: `(proofs . ((<start> . <end>) ...))`
(define (run-egg runner cmd)
  ;; Run egg using runner
  (define ctx (egg-runner-ctx runner))
  (define-values (root-ids egg-graph)
    (egraph-run-schedule (egg-runner-exprs runner) (egg-runner-schedule runner) ctx))
  ; Perform extraction
  (match cmd
    [`(single . ,extractor) ; single expression extraction
     (define regraph (make-regraph egg-graph))
     (define exprs '())
     (define values (make-hash))
     (define extract-id (extractor regraph))
     (define reprs (egg-runner-reprs runner))

     (define l
       (for/list ([id (in-list root-ids)] ; iterate through eclasses
                  [repr (in-list reprs)])
         (define eclass (regraph-extract-variants regraph extract-id id repr)) ; define an eclass
         (set! exprs (cons (first eclass) exprs)) ;add an expression from each eclass
         (regraph-extract-best regraph extract-id id repr)))

     (define (strip-approx prog) ;helper function to get expression
       (match prog
         [(approx _ impl) (strip-approx impl)]
         [(list op args ...) (cons op (map strip-approx args))]
         [_ prog]))
     (define duplicatesHash (make-hash))
     (define duplicates (list))
     (for ([expr exprs])
       (if (hash-has-key? duplicatesHash expr)
           (set! duplicates (cons expr duplicates))
           (hash-set! duplicatesHash expr 0)))
     ;(displayln (format "duplicates:~a" duplicates))
     (remove-duplicates exprs)

     (define (run-rival pts)
       (for ([pt pts])
         ;(displayln pt)
         (define var-hash (make-hash))
         (for ([key (hash-keys var-hash)])
           (hash-set! hash key 0))
         (for ([expr exprs])
           (set! expr (strip-approx expr))
           (define repr (get-representation 'binary64))
           (define vars (free-variables expr))
           (define ctx (context vars repr (map (const repr) vars)))
           (define compiler
             (make-real-compiler (list (prog->spec expr)) (list ctx))) ; Create variables
           (define list-pts (list))
           (for ([var vars])
             (hash-ref var-hash
                       var
                       (lambda ()
                         (hash-set! var-hash var (car pt))
                         (set! pt (cdr pt))
                         "0")))
           (for ([var vars])
             (set! list-pts (cons (hash-ref var-hash var) list-pts)))
           (define-values (status value) (real-apply compiler (reverse list-pts)))
           (if (and (hash-has-key? values expr) (equal? status 'valid))
               (hash-set! values expr (cons value (hash-ref values expr)))
               (hash-set! values expr (list value))))))

     (if (not (equal? (*pcontext*) #f))
         (run-rival (for/list ([(pt ex) (in-pcontext (*pcontext*))])
                      pt))
         (run-rival (list (list 91103.09230133967
                                1.4903069331671184e-261
                                1.730184674933312e-88
                                6.8792919307949795e-242
                                1.4903069331671184))))

     (for ([val-point (hash-map values cons)])
       (for ([val-point2 (hash-map values cons)])
       
         (define prog `(-.f64))
         (set! prog (cons (car val-point) prog))
         (set! prog (cons (car val-point2) prog))
         (set! prog (reverse prog))

         (define expr1 (car val-point))
         (define expr2 (car val-point2))

         (define repr (get-representation 'binary64))
         (define vars
           (remove-duplicates (append (free-variables (car val-point))
                                      (free-variables (car val-point2)))))
         (define ctx (context vars repr (map (const repr) vars)))
         (define pts (list 1 10 20 30 40))

         (define-values (status diff)
           (real-apply (make-real-compiler (list (prog->spec prog)) (list ctx)) pts))
          ; Checks both status and val-points
         (when (and (equal? (cdr val-point) (cdr val-point2))
                    (not (equal? (car val-point) (car val-point2))))
           (displayln "__________________")
           (display "prog:")
           (displayln prog)
           (display "diff:")
           (displayln diff)
           (when (not (equal? diff #f))
           (if (equal? (first diff) -0.0)
               (format "Expr 1: ~a Expr 2: ~a" (car val-point) (car val-point2));insert match statement to check for patterns ex- x+0 -> x
               (displayln "NO"))))))
     l]
    [`(multi . ,extractor) ; multi expression extraction
     (define regraph (make-regraph egg-graph))
     (define extract-id (extractor regraph))
     (define reprs (egg-runner-reprs runner))
     (for/list ([id (in-list root-ids)] [repr (in-list reprs)])
       (regraph-extract-variants regraph extract-id id repr))]
    [`(proofs . ((,start-exprs . ,end-exprs) ...)) ; proof extraction
     (for/list ([start (in-list start-exprs)] [end (in-list end-exprs)])
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
     (for/list ([start (in-list start-exprs)] [end (in-list end-exprs)])
       (egraph-expr-equal? egg-graph start end ctx))]
    [_ (error 'run-egg "unknown command `~a`\n" cmd)]))
