#lang racket

(require "../utils/common.rkt"
         "compiler.rkt"
         "../utils/float.rkt"
         "../syntax/types.rkt")

(provide *pcontext*
         in-pcontext
         mk-pcontext
         for/pcontext
         pcontext?
         pcontext-points
         json->pcontext
         pcontext->json
         split-pcontext
         pcontext-length
         errors
         batch-errors
         errors-score)

;; pcontexts are Herbie's standard data structure for storing
;; ground-truth information. They contain 1) a set of sampled input
;; points; and 2) a ground-truth output for each input.

(define *pcontext* (make-parameter #f))
(struct pcontext (points exacts) #:prefab)

(define (in-pcontext pcontext)
  (in-parallel (in-vector (pcontext-points pcontext)) (in-vector (pcontext-exacts pcontext))))

(define (pcontext-length pcontext)
  (vector-length (pcontext-points pcontext)))

(define/contract (mk-pcontext points exacts)
  (-> (non-empty-listof (listof any/c)) (non-empty-listof any/c) pcontext?)
  (pcontext (list->vector points) (list->vector exacts)))

(define-syntax-rule (for/pcontext ([(pt ex) pcontext] other ...) body ...)
  (let-values ([(pts* exs*)
                (for/lists (pts* exs*) ([(pt ex) (in-pcontext pcontext)] other ...) body ...)])
    (mk-pcontext pts* exs*)))

(define (split-pcontext pctx num-a num-b)
  (match-define (pcontext pts exs) pctx)
  (unless (= (+ num-a num-b) (vector-length pts))
    (error 'split-pcontext
           "Cannot split pcontext of size ~a into ~a and ~a"
           (vector-length pts)
           num-a
           num-b))
  (define-values (pts-a pts-b) (vector-split-at pts num-a))
  (define-values (exs-a exs-b) (vector-split-at exs num-a))
  (values (pcontext pts-a exs-a) (pcontext pts-b exs-b)))

;; Herbie's standard error measure is the average bits of error across
;; all points in a pcontext.

(define (average . s)
  (/ (apply + s) (length s)))

(define (errors-score e)
  (apply average (map ulps->bits e)))

(define (errors expr pcontext ctx)
  (first (batch-errors (list expr) pcontext ctx)))

(define (batch-errors exprs pcontext ctx)
  (define fn (compile-progs exprs ctx))
  (define repr (context-repr ctx))
  (define special? (representation-special-value? repr))
  (define max-error (+ 1 (expt 2 (representation-total-bits repr))))

  ;; This generates the errors array in reverse because that's how lists work
  (define num-points (pcontext-length pcontext))
  (for/fold ([result (make-list (length exprs) '())])
            ([point (in-vector (pcontext-points pcontext) (- num-points 1) -1 -1)]
             [exact (in-vector (pcontext-exacts pcontext) (- num-points 1) -1 -1)])
    (for/list ([out (in-vector (apply fn point))]
               [rest (in-list result)])
      (cons (if (special? out)
                max-error
                (ulp-difference out exact repr))
            rest))))

;; Herbie <=> JSON conversion for pcontext
;; A JSON pcontext is just a list of lists
;; ((pt1 ex1) (pt2 ex2) ...)

(define (json->pcontext json ctx)
  (define output-repr (context-repr ctx))
  (define var-reprs (context-var-reprs ctx))
  (define-values (pts exs)
    (for/lists (pts exs)
               ([entry (in-list json)])
               (match-define (list pt ex) entry)
               (values (map real->repr pt var-reprs)
                       (real->repr (json->value ex output-repr) output-repr))))
  (mk-pcontext pts exs))

(define (pcontext->json pcontext repr)
  (for/list ([(pt ex) (in-pcontext pcontext)])
    (list (map (curryr value->json repr) pt) (value->json ex repr))))
