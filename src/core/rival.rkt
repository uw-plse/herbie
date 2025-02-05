;; A narrow shim for Rival's "machine" abstraction.
;; A Rival "machine" performs real evaluation for multiple expressions on a point.
;;
;; Ensure this file has minimal dependencies since `<herbie>/syntax/syntax.rkt`
;; requires the file to synthesize floating-point implementations!
;;

#lang racket

(require rival)

(require "../config.rkt"
         "../syntax/types.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt")

(provide (struct-out real-compiler)
         (contract-out
          [make-real-compiler
           (->i ([es (listof any/c)]
                 [ctxs (es) (and/c unified-contexts? (lambda (ctxs) (= (length es) (length ctxs))))])
                (#:pre [pre any/c])
                [c real-compiler?])]
          [real-apply (-> real-compiler? list? (values symbol? any/c))]
          [real-compiler-clear! (-> real-compiler-clear! void?)]
          [real-compiler-analyze (-> real-compiler? (vectorof ival?) ival?)]))

(define (unified-contexts? ctxs)
  (and ((non-empty-listof context?) ctxs)
       (let ([ctx0 (car ctxs)])
         (for/and ([ctx (in-list (cdr ctxs))])
           (and (equal? (context-vars ctx0) (context-vars ctx))
                (for/and ([var (in-list (context-vars ctx0))])
                  (equal? (context-lookup ctx0 var) (context-lookup ctx var))))))))

(define (expr-size expr)
  (if (list? expr)
      (apply + 1 (map expr-size (cdr expr)))
      1))

(define (repr->discretization repr)
  (discretization (representation-total-bits repr)
                  (representation-bf->repr repr)
                  (lambda (x y) (- (ulp-difference x y repr) 1))))

;; Herbie's wrapper around the Rival machine abstraction.
(struct real-compiler (pre vars var-reprs exprs reprs machine))

;; Creates a Rival machine.
;; Takes a context to encode input variables and their representations,
;; a list of expressions, and a list of output representations
;; for each expression. Optionally, takes a precondition.
(define (make-real-compiler specs ctxs #:pre [pre '(TRUE)])
  (define vars (context-vars (first ctxs)))
  (define var-reprs (context-var-reprs (first ctxs)))
  (define reprs (map context-repr ctxs))
  ; create the machine
  (define exprs (cons `(assert ,pre) specs))
  (define discs (cons boolean-discretization (map repr->discretization reprs)))
  (define machine (rival-compile exprs vars discs))
  (timeline-push! 'compiler
                  (apply + 1 (expr-size pre) (map expr-size specs))
                  (+ (length vars) (rival-profile machine 'instructions)))
  ; wrap it with useful information for Herbie
  (real-compiler pre vars var-reprs specs reprs machine))

;; Runs a Rival machine on an input point.
(define (real-apply compiler pt)
  (match-define (real-compiler _ vars var-reprs _ _ machine) compiler)
  (define start (current-inexact-milliseconds))
  (define pt*
    (for/vector #:length (length vars)
                ([val (in-list pt)]
                 [repr (in-list var-reprs)])
      ((representation-repr->bf repr) val)))
  (define-values (status value)
    (with-handlers ([exn:rival:invalid? (lambda (e) (values 'invalid #f))]
                    [exn:rival:unsamplable? (lambda (e) (values 'exit #f))])
      (parameterize ([*rival-max-precision* (*max-mpfr-prec*)]
                     [*rival-max-iterations* 5])
        (values 'valid (rest (vector->list (rival-apply machine pt*))))))) ; rest = drop precondition
  (when (> (rival-profile machine 'bumps) 0)
    (warn 'ground-truth
          "Could not converge on a ground truth"
          #:extra (for/list ([var (in-list vars)]
                             [val (in-list pt)])
                    (format "~a = ~a" var val))))
  (define executions (rival-profile machine 'executions))
  (when (>= (vector-length executions) (*rival-profile-executions*))
    (warn 'profile "Rival profile vector overflowed, profile may not be complete"))
  (define prec-threshold (exact-floor (/ (*max-mpfr-prec*) 25)))
  (for ([execution (in-vector executions)])
    (define name (symbol->string (execution-name execution)))
    (define precision
      (- (execution-precision execution) (remainder (execution-precision execution) prec-threshold)))
    (timeline-push!/unsafe 'mixsample (execution-time execution) name precision))
  (timeline-push!/unsafe 'outcomes
                         (- (current-inexact-milliseconds) start)
                         (rival-profile machine 'iterations)
                         (~a status)
                         1)
  (values status value))

;; Clears profiling data.
(define (real-compiler-clear! compiler)
  (rival-profile (real-compiler-machine compiler) 'executions)
  (void))

;; Returns whether the machine is guaranteed to raise an exception
;; for the given inputs range. The result is an interval representing
;; how certain the result is: no, maybe, yes.
(define (real-compiler-analyze compiler input-ranges)
  (rival-analyze (real-compiler-machine compiler) input-ranges))
