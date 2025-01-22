#lang racket

(require ffi/unsafe
         ffi/unsafe/define
         ffi/vector
         racket/runtime-path)

(provide egraph_create
         egraph_destroy
         egraph_add_expr
         egraph_add_node
         egraph_run
         egraph_copy
         egraph_get_stop_reason
         egraph_find
         egraph_serialize
         egraph_get_eclasses
         egraph_get_eclass
         egraph_get_simplest
         egraph_get_variants
         egraph_get_cost
         egraph_is_unsound_detected
         egraph_get_times_applied
         egraph_get_proof
         (struct-out iteration-data)
         (struct-out FFIRule)
         make-ffi-rule)

(define-runtime-path libeggmath-path
                     (build-path "target/release"
                                 (string-append (case (system-type)
                                                  [(windows) "egg_math"]
                                                  [else "libegg_math"])
                                                (bytes->string/utf-8 (system-type 'so-suffix)))))

; Checks if Racket is being run in emulation via rosetta
(define (running-on-rosetta?)
  (and (equal? (system-type 'os) 'macosx)
       (equal? (system-type 'arch) 'x86_64)
       (equal? (with-output-to-string (lambda () ; returns true if running in emulation
                                        (system "sysctl -n sysctl.proc_translated")))
               "1\n")))

; Note, this message should not be reached.
(define fallback-message
  (string-append "Error: unable to load the 'egg-math' library\n"
                 "Please file a bug at https://github.com/herbie-fp/herbie/issues"))

; Note, refering to ARM as Apple Silicon to match Racket download page.
(define rosetta-message
  (string-append "Error: You are running the 'x86' version of Racket via 'Rosetta' emulation.\n"
                 "  Please use the 'Apple Silicon' version of Racket instead.\n"
                 "You can install it from https://download.racket-lang.org"))

(define (handle-eggmath-import-failure)
  (define error-message (if (running-on-rosetta?) rosetta-message fallback-message))
  (raise-user-error error-message))

; Checks for a condition on MacOS if x86 Racket is being used on an ARM mac.
(define-ffi-definer define-eggmath (ffi-lib libeggmath-path #:fail handle-eggmath-import-failure))

;; Frees a Rust-allocated C-string
(define-eggmath destroy_string (_fun _pointer -> _void))

;; Gets the length of a Rust-allocated C-string in bytes,
;; excluding the nul terminator.
(define-eggmath string_length (_fun _pointer -> _uint32))

;; Converts a Racket string to a C-style string.
(define (string->_rust/string s #:raw? [raw? #f])
  (define bstr (string->bytes/utf-8 s))
  (define n (bytes-length bstr))
  (define p (malloc (if raw? 'raw 'atomic) (add1 n)))
  (memcpy p bstr n)
  (ptr-set! p _byte n 0)
  p)

;; Converts a non-NULL, Rust-allocated C-string to a Racket string,
;; freeing the Rust string.
(define (_rust/string->string p)
  (define len (string_length p))
  (define bstr (make-bytes len))
  (memcpy bstr p len)
  (destroy_string p)
  (bytes->string/utf-8 bstr))

;; Converts a non-NULL, Rust-allocated C-string to a Racket datum
;; by repeatedly reading the string. The underlying Rust string
;; is automatically freed.
(define (_rust/string->data p)
  (define len (string_length p))
  (define bstr (make-bytes len))
  (memcpy bstr p len)
  (destroy_string p)
  (for/list ([datum (in-port read (open-input-bytes bstr))])
    datum))

;; FFI type that converts Rust-allocated C-style strings
;; to Racket strings, automatically freeing the Rust-side allocation.
(define _rust/string
  (make-ctype _pointer
              (lambda (x) (and x (string->_rust/string x)))
              (lambda (x) (and x (_rust/string->string x)))))

;; FFI type that converts Rust-allocated C-style strings
;; to a Racket datum via `read`, automatically freeing the Rust-side allocation.
(define _rust/datum
  (make-ctype _pointer
              (lambda (x) (and x (string->_rust/string (~a x))))
              (lambda (x) (and x (first (_rust/string->data x))))))

;; FFI type that converts Rust-allocated C-style strings
;; to multiple Racket datum via reapeated use of `read`,
;; automatically freeing the Rust-side allocation.
(define _rust/data
  (make-ctype _pointer
              (lambda (_) (error '_rust/data "cannot be used as an input type"))
              (lambda (x) (and x (_rust/string->data x)))))

; Egraph iteration data
; Not managed by Racket GC.
; Must call `destroy_egraphiters` to free.
(define-cstruct _EGraphIter ([numnodes _uint] [numeclasses _uint] [time _double]) #:malloc-mode 'raw)

;; Frees an array of _EgraphIter structs
(define-eggmath destroy_egraphiters (_fun _pointer -> _void))

;; Racket representation of `_EGraphIter`
(struct iteration-data (num-nodes num-eclasses time))

;; Rewrite rule that can be passed over the FFI boundary.
;; Must be manually freed.
(define-cstruct _FFIRule ([name _pointer] [left _pointer] [right _pointer]) #:malloc-mode 'raw)

;; Constructs for `_FFIRule` struct.
(define (make-ffi-rule name lhs rhs)
  (define name* (string->_rust/string (~a name) #:raw? #t))
  (define lhs* (string->_rust/string (~a lhs) #:raw? #t))
  (define rhs* (string->_rust/string (~a rhs) #:raw? #t))
  (define p (make-FFIRule name* lhs* rhs*))
  (register-finalizer p free-ffi-rule)
  p)

;; Frees a `_FFIRule` struct.
(define (free-ffi-rule rule)
  (free (FFIRule-name rule))
  (free (FFIRule-left rule))
  (free (FFIRule-right rule))
  (free rule))

; GC'able egraph
; If Racket GC can prove unreachable, `egraph_destroy` will be called
(define _egraph-pointer
  (_cpointer 'egraph
             #f
             #f
             (lambda (p)
               (register-finalizer p egraph_destroy)
               p)))

;; Constructs an e-graph instances.
(define-eggmath egraph_create (_fun _uint -> _egraph-pointer))

;; Frees an e-graph instance.
(define-eggmath egraph_destroy (_fun _egraph-pointer -> _void))

;; Copies an e-graph instance.
(define-eggmath egraph_copy (_fun _egraph-pointer -> _egraph-pointer))

;; Adds an expression to the e-graph.
;; egraph -> expr -> id
(define-eggmath egraph_add_expr (_fun _egraph-pointer _rust/datum -> _uint))

; egraph -> string -> ids -> bool -> id
(define-eggmath egraph_add_node
                (_fun [p : _egraph-pointer] ; egraph
                      [f : _rust/string] ; enode op
                      [v : _u32vector] ; id vector
                      [_uint = (u32vector-length v)] ; id vector length
                      [is_root : _stdbool] ; root node?
                      ->
                      _uint))

(define-eggmath egraph_is_unsound_detected (_fun _egraph-pointer -> _stdbool))

;; Runs the egraph with a set of rules, returning the statistics of the run.
(define-eggmath egraph_run
                (_fun _egraph-pointer ;; egraph
                      (ffi-rules : (_list i _FFIRule-pointer)) ;; ffi rules
                      (_uint = (length ffi-rules)) ;; number of rules
                      (iterations-length : (_ptr o _uint)) ;; pointer to length of resulting array
                      (iterations-ptr :
                                      (_ptr o _pointer)) ;; pointer to array allocation, caller frees
                      _uint ;; iter limit
                      _uint ;; node limit
                      _stdbool ;; simple scheduler?
                      _stdbool ;; constant folding enabled?
                      ->
                      (iterations : _EGraphIter-pointer) ;; array of _EgraphIter structs
                      ->
                      (begin
                        (define iter-data
                          (for/list ([i (in-range iterations-length)])
                            (define ptr (ptr-add iterations i _EGraphIter))
                            (iteration-data (EGraphIter-numnodes ptr)
                                            (EGraphIter-numeclasses ptr)
                                            (EGraphIter-time ptr))))
                        (destroy_egraphiters iterations-ptr)
                        iter-data)))

;; gets the stop reason as an integer
(define-eggmath egraph_get_stop_reason (_fun _egraph-pointer -> _uint))

;; egraph -> string
(define-eggmath egraph_serialize (_fun _egraph-pointer -> _rust/datum))

;; egraph -> uint
(define-eggmath egraph_size (_fun _egraph-pointer -> _uint))

;; egraph -> id -> uint
(define-eggmath egraph_eclass_size (_fun _egraph-pointer _uint -> _uint))

;; egraph -> id -> idx -> uint
(define-eggmath egraph_enode_size (_fun _egraph-pointer _uint _uint -> _uint))

;; egraph -> u32vector
(define-eggmath
 egraph_get_eclasses
 (_fun [e : _egraph-pointer] [v : _u32vector = (make-u32vector (egraph_size e))] -> _void -> v))

;; egraph -> id -> u32 -> (or symbol? number? (cons symbol u32vector))
;; UNSAFE: `v` must be large enough to contain the child ids
(define-eggmath egraph_get_node
                (_fun [e : _egraph-pointer]
                      [id : _uint32]
                      [idx : _uint32]
                      [v : _u32vector]
                      ->
                      [f : _rust/string]
                      ->
                      (if (zero? (u32vector-length v))
                          (or (string->number f) (string->symbol f))
                          (cons (string->symbol f) v))))
; u32vector
(define empty-u32vec (make-u32vector 0))

; egraph -> id -> (vectorof (or symbol? number? (cons symbol u32vector)))
(define (egraph_get_eclass egg-ptr id)
  (define n (egraph_eclass_size egg-ptr id))
  (for/vector #:length n
              ([i (in-range n)])
    (define node-size (egraph_enode_size egg-ptr id i))
    (if (zero? node-size)
        (egraph_get_node egg-ptr id i empty-u32vec)
        (egraph_get_node egg-ptr id i (make-u32vector node-size)))))

;; egraph -> id -> id
(define-eggmath egraph_find (_fun _egraph-pointer _uint -> _uint))

;; egraph -> id -> (listof expr)
(define-eggmath egraph_get_simplest
                (_fun _egraph-pointer
                      _uint ;; node id
                      _uint ;; iteration
                      ->
                      _rust/datum)) ;; expr

;; egraph -> id -> string -> (listof expr)
(define-eggmath egraph_get_variants
                (_fun _egraph-pointer
                      _uint ;; node id
                      _rust/datum ;; original expr
                      ->
                      _rust/data)) ;; listof expr

;; egraph -> string -> string -> string
;; TODO: in Herbie, we bail on converting the proof
;; if the string is too big. It would be more efficient
;; to bail here instead.
(define-eggmath egraph_get_proof
                (_fun _egraph-pointer ;; egraph
                      _rust/datum ;; expr1
                      _rust/datum ;; expr2
                      ->
                      _rust/string)) ;; string

(define-eggmath egraph_get_cost
                (_fun _egraph-pointer
                      _uint ;; node id
                      _uint ;; iteration
                      ->
                      _uint))

(define-eggmath egraph_get_times_applied
                (_fun _egraph-pointer
                      _pointer ;; name of the rule
                      ->
                      _uint))
