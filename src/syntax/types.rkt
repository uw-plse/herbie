#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt")

(provide type-name?
         (struct-out representation)
         get-representation
         repr-exists?
         repr->symbol
         repr->prop
         (struct-out context)
         *context*
         context-extend
         context-append
         context-lookup)

(module+ internals
  (provide define-type
           define-representation
           register-generator!
           register-representation!
           register-representation-alias!))

;; Types

(define type-dict (make-hasheq))
(define (type-name? x)
  (hash-has-key? type-dict x))

(define-syntax-rule (define-type name _ ...)
  (hash-set! type-dict 'name #t))

(define-type real)
(define-type bool)

;; Representations

(struct representation
        (name type repr? bf->repr repr->bf ordinal->repr repr->ordinal total-bits special-value?)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc repr port mode)
     (fprintf port "#<representation ~a>" (representation-name repr)))])

(define representations (hash))

;; Representation name sanitizer
(define (repr->symbol repr)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define repr-name (representation-name repr))
  (string->symbol (string-replace* (~a repr-name) replace-table)))

;; Converts a representation into a rounding property
(define (repr->prop repr)
  (match (representation-type repr)
    ['bool '()]
    ['real (list (cons ':precision (representation-name repr)))]))

;; Repr / operator generation
;; Some plugins might define 'parameterized' reprs (e.g. fixed point with
;; m integer and n fractional bits). Since defining an infinite number of reprs
;; is impossible, Herbie stores a list of 'repr generators' to query if it comes
;; across a repr that is not known at the time.

;; Generators take one argument, a repr name, and returns true if knows what the
;; repr is and has generated that repr and its operators, and false otherwise
(define repr-generators '())
(define *current-generator* (make-parameter #f))

(define/contract (register-generator! proc)
  (-> (-> any/c any/c) void?)
  (unless (set-member? repr-generators proc)
    (set! repr-generators (cons proc repr-generators))))

;; Queries each plugin to generate the representation
(define (generate-repr repr-name)
  (or (hash-has-key? representations repr-name)
      (for/or ([proc repr-generators])
        ;; Check if a user accidently created an infinite loop in their plugin!
        (when (and (eq? proc (*current-generator*)) (not (hash-has-key? representations repr-name)))
          (raise-herbie-error
           (string-append
            "Tried to generate `~a` representation while generating the same representation. "
            "Check your plugin to make sure you register your representation(s) "
            "before calling `get-representation`!")
           repr-name))
        (parameterize ([*current-generator* proc])
          (proc repr-name)))))

;; Returns the representation associated with `name`
;; attempts to generate the repr if not initially found
(define (get-representation name)
  (or (hash-ref representations name #f)
      (and (generate-repr name) (hash-ref representations name #f))
      (raise-herbie-error "Could not find support for ~a representation: ~a"
                          name
                          (string-join (map ~s (hash-keys representations)) ", "))))

(define (repr-exists? name)
  (hash-has-key? representations name))

;; Registers a representation that can be invoked with ':precision <name>'.
;; Creates a new representation with the given traits and associates it
;; with the same name. See `register-representation-alias!` for associating
;; a representation with a different name.
(define (register-representation! name type repr? . args)
  (unless (type-name? type)
    (raise-herbie-error "Tried to register a representation for type ~a: not found" type))
  (define repr (apply representation name type repr? args))
  (set! representations (hash-set representations name repr)))

;; Associates an existing representation with a (possibly different) name.
;; Useful for defining an common alias for an equivalent representation,
;; e.g. float for binary32.
(define (register-representation-alias! name repr)
  (unless (representation? repr)
    (raise-herbie-error "Tried to register an alias for representation ~a: not found"
                        (representation-name repr)))
  (set! representations (hash-set representations name repr)))

(define-syntax-rule (define-representation (name type repr?) args ...)
  (register-representation! 'name 'type repr? args ...))

;; Contexts

(struct context (vars repr var-reprs) #:transparent)

;; Current context
(define *context* (make-parameter #f))

(define (context-extend ctx var repr)
  (struct-copy context
               ctx
               [vars (cons var (context-vars ctx))]
               [var-reprs (cons repr (context-var-reprs ctx))]))

(define (context-append ctx var repr)
  (struct-copy context
               ctx
               [vars (append (context-vars ctx) (list var))]
               [var-reprs (append (context-var-reprs ctx) (list repr))]))

(define (context-lookup ctx var)
  (dict-ref (map cons (context-vars ctx) (context-var-reprs ctx)) var))
