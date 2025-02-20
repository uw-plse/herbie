#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "../core/programs.rkt"
         "../core/rules.rkt"
         "matcher.rkt"
         "syntax.rkt"
         "types.rkt")

(provide define-platform
         *active-platform*
         activate-platform!
         platform-simplify-rules
         platform-lifting-rules
         platform-lowering-rules

         get-fpcore-impl
         ;; Platform API
         ;; Operator sets
         (contract-out ;; Platforms
          [platform? (-> any/c boolean?)]
          [platform-name (-> platform? any/c)]
          [platform-reprs (-> platform? (listof representation?))]
          [platform-impls (-> platform? (listof symbol?))]
          [platform-union (-> platform? platform? ... platform?)]
          [platform-intersect (-> platform? platform? ... platform?)]
          [platform-subtract (-> platform? platform? ... platform?)]
          ; Cost model
          [platform-impl-cost (-> platform? any/c any/c)]
          [platform-repr-cost (-> platform? any/c any/c)]
          [platform-node-cost-proc (-> platform? procedure?)]
          [platform-cost-proc (-> platform? procedure?)]))

(module+ internals
  (provide register-platform!))

;;; Platforms describe a set of representations, operator, and constants
;;; Herbie should use during its improvement loop. Platforms are just
;;; a "type signature" - they provide no implementations of floating-point
;;; operations (see plugins). During runtime, platforms will verify if
;;; every listed feature is actually loaded by Herbie and will panic if
;;; implemenations are missing. Unlike plugins, only one platform may be
;;; active at any given time and platforms may be activated or deactivated.
;;;
;;; A small API is provided for platforms for querying the supported
;;; operators, operator implementations, and representation conversions.
(struct platform (name reprs impls impl-costs repr-costs)
  #:name $platform
  #:constructor-name create-platform
  #:methods gen:custom-write
  [(define (write-proc p port mode)
     (if (platform-name p)
         (fprintf port "#<platform:~a>" (platform-name p))
         (fprintf port "#<platform>")))])

;; Platform table, mapping name to platform
(define platforms (make-hash))

;; Active platform
(define *active-platform* (make-parameter #f))

;; Loads a platform.
(define (activate-platform! name)
  (define pform (hash-ref platforms name #f))

  (unless pform
    (raise-herbie-error "unknown platform `~a`, found (~a)"
                        name
                        (string-join (map ~a (hash-keys platforms)) ", ")))

  (*platform-name* name)
  (*active-platform* pform))

;; Registers a platform under identifier `name`.
(define (register-platform! name pform)
  (when (hash-has-key? platforms name)
    (error 'register-platform! "platform already registered ~a" name))
  (hash-set! platforms name (struct-copy $platform pform [name name])))

;; Constructor procedure for platforms.
;; The platform is described by a list of implementations.
;;
;;  [<name>  (<cost> | #f)]
;;
;; Verifies that a platform is well-formed and the platform will be
;; supported by Herbie's runtime. A platform specified by `optional?`
;; may discard any implementations it fails to find in the Racket runtime.
(define (make-platform pform
                       literals
                       #:optional? [optional? #f]
                       #:if-cost [if-cost #f]
                       #:default-cost [default-cost #f])
  (define costs (make-hash))
  (define missing (mutable-set))
  (define impls
    (reap
     [sow]
     (for ([impl-sig (in-list pform)])
       (match-define (list impl cost) impl-sig)
       (define final-cost cost)
       (unless (or cost default-cost)
         (raise-herbie-error "Missing cost for ~a" impl))
       (unless cost
         (set! final-cost default-cost))
       (cond
         [(impl-exists? impl)
          (hash-set! costs impl final-cost)
          (sow impl)]
         [optional? (set-add! missing impl)]
         [else (raise-herbie-missing-error "Missing implementation ~a required by platform" impl)]))))
  (define reprs
    (remove-duplicates (apply append
                              (for/list ([impl (in-list impls)])
                                `(,@(impl-info impl 'itype) ,(impl-info impl 'otype))))))

  (define repr-costs (make-hash))
  (for ([literal (in-list literals)])
    (match-define (list repr cost) literal)
    (cond
      [(repr-exists? repr)
       (if (hash-has-key? repr-costs repr)
           (raise-herbie-error "Duplicate literal ~a" repr)
           (hash-set! repr-costs (get-representation repr) cost))]
      [else (raise-herbie-missing-error "Missing representation ~a required by platform" repr)]))
  ; set cost of `if`
  (when if-cost
    (hash-set! costs 'if if-cost))
  ; emit warnings if need be
  (unless (set-empty? missing)
    (warn 'platform
          "platform has missing optional implementations: ~a"
          (string-join (for/list ([m (in-set missing)])
                         (format "~a" m))
                       " ")))
  (create-platform #f reprs impls (make-immutable-hash (hash->list costs)) repr-costs))

(begin-for-syntax
  ;; Parse if cost syntax
  (define (platform/parse-if-cost stx)
    (syntax-case stx (max sum)
      [(max x) #'(list 'max x)]
      [(sum x) #'(list 'sum x)]
      [x #'(list 'max x)])))

;; Macro version of `make-platform`
;;
;; Example usage:
;; ```
;; (define-platform default
;;   (platform
;;     #:literal [binary64 64]                  ; literal representation with cost
;;     #:literal [binary32 32]                  ; literal representation with cost
;;     #:default-cost 1                         ; default cost per impl
;;     #:if-cost 1                              ; cost of an if branch (using max strategy)
;;     [fabs.f64 3]
;;     fabs.f32
;;     ...
;; ))
;; ```
(define-syntax (define-platform stx)
  (define (oops! why [sub-stx #f])
    (raise-syntax-error 'platform why stx sub-stx))
  (syntax-case stx ()
    [(_ id cs ...)
     (let ([if-cost #f]
           [default-cost #f]
           [optional? #f])
       (let loop ([cs #'(cs ...)]
                  [impls '()]
                  [costs '()]
                  [reprs '()]
                  [repr-costs '()])
         (syntax-case cs ()
           [()
            (let ([platform-id #'id])
              (unless (identifier? platform-id)
                (oops! "platform id is not a valid identifier" platform-id))
              (with-syntax ([platform-id platform-id]
                            [(impls ...) (reverse impls)]
                            [(costs ...) (reverse costs)]
                            [(reprs ...) reprs]
                            [(repr-costs ...) repr-costs]
                            [if-cost if-cost]
                            [default-cost default-cost]
                            [optional? optional?])
                #'(define platform-id
                    (make-platform `([impls ,costs] ...)
                                   `([reprs ,repr-costs] ...)
                                   #:optional? optional?
                                   #:if-cost if-cost
                                   #:default-cost default-cost))))]
           [(#:if-cost cost rest ...)
            (cond
              [if-cost (oops! "multiple #:if-cost clauses" stx)]
              [else
               (set! if-cost (platform/parse-if-cost #'cost))
               (loop #'(rest ...) impls costs reprs repr-costs)])]
           [(#:if-cost) (oops! "expected value after keyword `#:if-cost`" stx)]
           [(#:default-cost cost rest ...)
            (cond
              [if-cost (oops! "multiple #:default-cost clauses" stx)]
              [else
               (set! default-cost #'cost)
               (loop #'(rest ...) impls costs reprs repr-costs)])]
           [(#:default-cost) (oops! "expected value after keyword `#:default-cost`" stx)]
           [(#:optional rest ...)
            (cond
              [optional? (oops! "multiple #:optional clauses" stx)]
              [else
               (set! optional? #t)
               (loop #'(rest ...) impls costs reprs repr-costs)])]
           [(#:literal [repr cost] rest ...)
            (loop #'(rest ...) impls costs (cons #'repr reprs) (cons #'cost repr-costs))]
           [(#:literals) (oops! "expected literals list after keyword `#:literals`" stx)]
           [([impl cost] rest ...)
            (loop #'(rest ...) (cons #'impl impls) (cons #'cost costs) reprs repr-costs)]
           [(impl rest ...) (loop #'(rest ...) (cons #'impl impls) (cons #f costs) reprs repr-costs)]
           [_ (oops! "bad syntax")])))]
    [_ (oops! "bad syntax")]))

;; Merger for costs.
(define (merge-cost pform-costs key #:optional? [optional? #f])
  (define costs (map (lambda (h) (hash-ref h key #f)) pform-costs))
  (match-define (list c0 cs ...) costs)
  (define cost
    (for/fold ([c0 c0]) ([c1 (in-list cs)])
      (match* (c0 c1)
        [(#f _) c1]
        [(_ #f) c0]
        [(c c) c]
        [(_ _) (error 'merge-costs "mismatch when combining cost model ~a ~a" key costs)])))
  (unless (or cost optional?)
    (error 'merge-costs "cannot find cost for implementation ~a" key))
  cost)

;; Set operations on platforms.
(define ((make-set-operation merge-impls) p1 . ps)
  ; apply set operation on impls
  (define impls (apply merge-impls (map platform-impls (cons p1 ps))))
  ; valid representations are based on impls
  (define reprs
    (remove-duplicates (for/fold ([reprs '()]) ([impl (in-list impls)])
                         (append (cons (impl-info impl 'otype) (impl-info impl 'itype)) reprs))))
  ; impl costs are based on impls
  (define pform-impl-costs (map platform-impl-costs (cons p1 ps)))
  (define impl-costs
    (for/hash ([impl (in-list impls)])
      (values impl (merge-cost pform-impl-costs impl))))
  ; special case for `if`
  (define if-cost (merge-cost pform-impl-costs 'if #:optional? #t))
  (when if-cost
    (set! impl-costs (hash-set impl-costs 'if if-cost)))
  ; repr costs are based on reprs (may be missing)
  (define pform-repr-costs (map platform-repr-costs (cons p1 ps)))
  (define repr-costs (hash))
  (for/list ([repr (in-list reprs)])
    (define repr-cost (merge-cost pform-repr-costs repr #:optional? #t))
    (when repr-cost
      (set! repr-costs (hash-set repr-costs repr repr-cost))))
  (create-platform #f reprs impls impl-costs repr-costs))

;; Set union for platforms.
;; Use list operations for deterministic ordering.
(define platform-union (make-set-operation (λ (rs . rss) (remove-duplicates (apply append rs rss)))))

;; Set intersection for platforms.
;; Use list operations for deterministic ordering.
(define platform-intersect
  (make-set-operation (λ (rs . rss)
                        (for/fold ([rs rs]) ([rs0 (in-list rss)])
                          (filter (curry set-member? (list->set rs0)) rs)))))

;; Set subtract for platforms.
;; Use list operations for deterministic ordering.
(define platform-subtract
  (make-set-operation (λ (rs . rss)
                        (for/fold ([rs rs]) ([rs0 (in-list rss)])
                          (filter-not (curry set-member? (list->set rs0)) rs)))))

; Implementation cost in a platform.
(define (platform-impl-cost pform impl)
  (hash-ref (platform-impl-costs pform)
            impl
            (lambda () (error 'platform-impl-cost "no cost for impl '~a" impl))))

; Representation (terminal) cost in a platform.
(define (platform-repr-cost pform repr)
  (hash-ref (platform-repr-costs pform)
            repr
            (lambda () (error 'platform-repr-cost "no cost for repr ~a" repr))))

; Cost model of a single node by a platform.
; Returns a procedure that must be called with the costs of the children.
(define (platform-node-cost-proc pform)
  (λ (expr repr)
    (match expr
      [(? literal?) (lambda () (platform-repr-cost pform repr))]
      [(? symbol?) (lambda () (platform-repr-cost pform repr))]
      [(list 'if _ _ _)
       (define if-cost (platform-impl-cost pform 'if))
       (lambda (cond-cost ift-cost iff-cost)
         (match if-cost
           [`(max ,n) (+ n cond-cost (max ift-cost iff-cost))]
           [`(sum ,n) (+ n cond-cost ift-cost iff-cost)]))]
      [(list impl args ...)
       (define impl-cost (platform-impl-cost pform impl))
       (lambda itype-costs
         (unless (= (length itype-costs) (length args))
           (error 'platform-node-cost-proc "arity mismatch, expected ~a arguments" (length args)))
         (apply + impl-cost itype-costs))])))

; Cost model parameterized by a platform.
(define (platform-cost-proc pform)
  (define bool-repr (get-representation 'bool))
  (define node-cost-proc (platform-node-cost-proc pform))
  (λ (expr repr)
    (let loop ([expr expr]
               [repr repr])
      (match expr
        [(? literal?) ((node-cost-proc expr repr))]
        [(? symbol?) ((node-cost-proc expr repr))]
        [(approx _ impl) (loop impl repr)]
        [(list 'if cond ift iff)
         (define cost-proc (node-cost-proc expr repr))
         (cost-proc (loop cond bool-repr) (loop ift repr) (loop iff repr))]
        [(list impl args ...)
         (define cost-proc (node-cost-proc expr repr))
         (define itypes (impl-info impl 'itype))
         (apply cost-proc (map loop args itypes))]))))

;; Rules from impl to spec (fixed for a particular platform)
(define/reset *lifting-rules* (make-hash))

;; Rules from spec to impl (fixed for a particular platform)
(define/reset *lowering-rules* (make-hash))

;; Synthesizes the LHS and RHS of lifting/lowering rules.
(define (impl->rule-parts impl)
  (define vars (impl-info impl 'vars))
  (define spec (impl-info impl 'spec))
  (values vars spec (cons impl vars)))

;; Synthesizes lifting rules for a given platform.
(define (platform-lifting-rules [pform (*active-platform*)])
  ;; every impl maps to a spec
  (define impls (platform-impls pform))
  (define impl-rules
    (for/list ([impl (in-list impls)])
      (hash-ref! (*lifting-rules*)
                 (cons impl pform)
                 (lambda ()
                   (define name (sym-append 'lift- impl))
                   (define itypes (impl-info impl 'itype))
                   (define otype (impl-info impl 'otype))
                   (define-values (vars spec-expr impl-expr) (impl->rule-parts impl))
                   (rule name impl-expr spec-expr (map cons vars itypes) otype '(lifting))))))
  ;; special rule for approx nodes
  ; (define approx-rule (rule 'lift-approx (approx 'a 'b) 'a '((a . real) (b . real)) 'real))
  ; (cons approx-rule impl-rules))
  impl-rules)

;; Synthesizes lowering rules for a given platform.
(define (platform-lowering-rules [pform (*active-platform*)])
  (define impls (platform-impls pform))
  (for/list ([impl (in-list impls)])
    (hash-ref! (*lowering-rules*)
               (cons impl pform)
               (lambda ()
                 (define name (sym-append 'lower- impl))
                 (define-values (vars spec-expr impl-expr) (impl->rule-parts impl))
                 (define itypes (map representation-type (impl-info impl 'itype)))
                 (define otype (representation-type (impl-info impl 'otype)))
                 (rule name spec-expr impl-expr (map cons vars itypes) otype '(lowering))))))

(define (expr-otype expr)
  (match expr
    [(? number?) #f]
    [(? variable?) #f]
    [(list 'if cond ift iff) (expr-otype ift)]
    [(list op args ...) (impl-info op 'otype)]))

(define (type-verify expr otype)
  (match expr
    [(? number?) '()]
    [(? variable?) (list (cons expr otype))]
    [(list 'if cond ift iff)
     (define bool-repr (get-representation 'bool))
     (define combined
       (merge-bindings (type-verify cond bool-repr)
                       (merge-bindings (type-verify ift otype) (type-verify iff otype))))
     (unless combined
       (error 'type-verify "Variable types do not match in ~a" expr))
     combined]
    [(list op args ...)
     (define op-otype (impl-info op 'otype))
     (when (not (equal? op-otype otype))
       (error 'type-verify "Operator ~a has type ~a, expected ~a" op op-otype otype))
     (define bindings '())
     (for ([arg (in-list args)]
           [itype (in-list (impl-info op 'itype))])
       (define combined (merge-bindings bindings (type-verify arg itype)))
       (unless combined
         (error 'type-verify "Variable types do not match in ~a" expr))
       (set! bindings combined))
     bindings]))

(define (expr->prog expr repr)
  (match expr
    [(? number?) (literal expr (representation-name repr))]
    [(? variable?) expr]
    [`(if ,cond ,ift ,iff)
     `(if ,(expr->prog cond (get-representation 'bool))
          ,(expr->prog ift repr)
          ,(expr->prog iff repr))]
    [`(,impl ,args ...)
     `(,impl ,@(for/list ([arg (in-list args)]
                          [itype (in-list (impl-info impl 'itype))])
                 (expr->prog arg itype)))]))

(define (impls-supported? expr)
  (match expr
    [(? number?) #t]
    [(? variable?) #t]
    [`(if ,cond ,ift ,iff)
     (and (impls-supported? cond) (impls-supported? ift) (impls-supported? iff))]
    [`(,impl ,args ...)
     (and (set-member? (platform-impls (*active-platform*)) impl) (andmap impls-supported? args))]))

(define (platform-simplify-rules)
  (reap [sow]
        (for ([impl (in-list (platform-impls (*active-platform*)))])
          (define rules (impl-info impl 'identities))
          (for ([identity (in-list rules)])
            (match identity
              [(list 'exact name expr)

               (when (impls-supported? expr)
                 (when (not (expr-otype expr))
                   (error "Exact identity expr cannot infer type"))
                 (define otype (expr-otype expr))
                 (define var-types (type-verify expr otype))
                 (define prog (expr->prog expr otype))
                 (define r
                   (rule name
                         prog
                         (prog->spec prog)
                         (for/hash ([binding (in-list var-types)])
                           (values (car binding) (cdr binding)))
                         (impl-info impl 'otype)
                         '()))
                 (sow r))]
              [(list 'commutes name expr rev-expr)
               (when (impls-supported? expr)
                 (define vars (impl-info impl 'vars))
                 (define itype (car (impl-info impl 'itype)))
                 (define otype (impl-info impl 'otype))
                 (define r
                   (rule name
                         (expr->prog expr otype)
                         (expr->prog rev-expr otype)
                         (for/hash ([v (in-list vars)])
                           (values v itype))
                         otype
                         '())) ; Commutes by definition the types are matching
                 (sow r))]
              [(list 'directed name lhs rhs)
               (when (and (impls-supported? lhs) (impls-supported? rhs))
                 (define lotype (expr-otype lhs))
                 (define rotype (expr-otype rhs))
                 (when (and (not lotype) (not rotype))
                   (error "Could not find type for lhs ~a and rhs ~a" lhs rhs))
                 (when (not lotype)
                   (set! lotype rotype))
                 (when (not rotype)
                   (set! rotype lotype))
                 (when (not (equal? lotype rotype))
                   (error "Incompatible types for lhs ~a and rhs ~a" lhs rhs))
                 (define var-types (merge-bindings (type-verify lhs lotype) (type-verify rhs rotype)))
                 (define r
                   (rule name
                         (expr->prog lhs lotype)
                         (expr->prog rhs rotype)
                         (for/hash ([binding (in-list var-types)])
                           (values (car binding) (cdr binding)))
                         (impl-info impl 'otype)
                         '()))
                 (sow r))])))))

;; Extracts the `fpcore` field of an operator implementation
;; as a property dictionary and expression.
(define (impl->fpcore impl)
  (match (impl-info impl 'fpcore)
    [(list '! props ... body) (values (props->dict props) body)]
    [body (values '() body)]))

;; For a given FPCore operator, rounding context, and input representations,
;; finds the best operator implementation. Panics if none can be found.
(define/contract (get-fpcore-impl op prop-dict ireprs)
  (-> symbol? prop-dict/c (listof representation?) (or/c symbol? #f))
  ; gather all implementations that have the same spec, input representations,
  ; and its FPCore translation has properties that are found in `prop-dict`
  (define impls
    (reap [sow]
          (for ([impl (in-list (platform-impls (*active-platform*)))]
                #:when (equal? ireprs (impl-info impl 'itype)))
            (define-values (prop-dict* expr) (impl->fpcore impl))
            (define expr*
              (if (symbol? expr)
                  (list expr)
                  expr)) ; Handle named constants
            (define pattern (cons op (map (lambda (_) (gensym)) ireprs)))
            (when (and (subset? prop-dict* prop-dict) (pattern-match pattern expr*))
              (sow impl)))))
  ; check that we have any matching impls
  (cond
    [(null? impls) #f]
    [else
     ; we rank implementations and select the highest scoring one
     (define scores
       (for/list ([impl (in-list impls)])
         (define-values (prop-dict* _) (impl->fpcore impl))
         (define num-matching (count (lambda (prop) (member prop prop-dict*)) prop-dict))
         (cons num-matching (- (length prop-dict) num-matching))))
     ; select the best implementation
     ; sort first by the number of matched properties,
     ; then tie break on the number of extraneous properties
     (match-define (list (cons _ best) _ ...)
       (sort (map cons scores impls)
             (lambda (x y)
               (cond
                 [(> (car x) (car y)) #t]
                 [(< (car x) (car y)) #f]
                 [else (> (cdr x) (cdr y))]))
             #:key car))
     best]))
