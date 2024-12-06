#lang racket

(require rackunit)
(require "../utils/common.rkt"
         "../utils/float.rkt"
         "rules.rkt"
         "../syntax/platform.rkt"
         "../syntax/load-plugin.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "compiler.rkt"
         "rival.rkt"
         "sampling.rkt")

(load-herbie-builtins)

(define num-test-points (make-parameter 100))

(define *conditions*
  (list '[asinh-2 . (>= x 0)]
        '[pow-unpow . (>= a 0)]
        '[pow-pow . (>= a 0)]
        '[sqrt-pow1 . (>= x 0)]
        '[asin-sin-s . (<= (fabs x) (/ (PI) 2))]
        '[acos-cos-s . (and (<= 0 x) (<= x (PI)))]
        '[atan-tan-s . (<= (fabs x) (/ (PI) 2))]))

(define double-repr (get-representation 'binary64))
(define boolean-repr (get-representation 'bool))

(define (type->repr type)
  (match type
    ['real double-repr]
    ['bool boolean-repr]))

(define (env->ctx env out)
  (define vars (dict-keys env))
  (define itypes (map type->repr (dict-values env)))
  (context vars (type->repr out) itypes))

(define (rule->impl-rules rule)
  (platform-impl-rules (list rule)))

(define (check-rule-sound test-rule)
  (match-define (rule name p1 p2 env out tags) test-rule)
  (define ctx (env->ctx env out))

  (match-define (list pts exs)
    (parameterize ([*num-points* (num-test-points)]
                   [*max-find-range-depth* 0])
      (cdr (sample-points '(TRUE) (list p1) (list ctx)))))

  (define compiler (make-real-compiler (list p2) (list ctx)))
  (for ([pt (in-list pts)]
        [v1 (in-list exs)])
    (with-check-info* (map make-check-info (context-vars ctx) pt)
                      (λ ()
                        (define-values (status v2) (real-apply compiler pt))
                        (with-check-info (['lhs v1] ['rhs v2] ['status status])
                                         (when (and (real? v2)
                                                    (nan? v2)
                                                    (not (set-member? '(exit unsamplable) status)))
                                           (fail "Right hand side returns NaN")))))))

(define (check-rule-correct test-rule)
  (match-define (rule name p1 p2 env out tags) test-rule)
  (define ctx (env->ctx env out))

  (define pre (dict-ref *conditions* name '(TRUE)))
  (match-define (list pts exs1 exs2)
    (parameterize ([*num-points* (num-test-points)]
                   [*max-find-range-depth* 0])
      (cdr (sample-points pre (list p1 p2) (list ctx ctx)))))

  (for ([pt (in-list pts)]
        [v1 (in-list exs1)]
        [v2 (in-list exs2)])
    (with-check-info* (map make-check-info (context-vars ctx) pt)
                      (λ ()
                        (with-check-info (['lhs v1] ['rhs v2])
                                         (check-eq? (ulp-difference v1 v2 (context-repr ctx)) 1))))))

(define (check-rule-fp-safe test-rule)
  (match-define (rule name p1 p2 itypes repr tags) test-rule)
  (define fv (dict-keys itypes))
  (define ctx (context fv repr (map (curry dict-ref itypes) fv)))
  (define (make-point _)
    (for/list ([v (in-list fv)])
      (random-generate (dict-ref itypes v))))
  (define points (build-list (num-test-points) make-point))
  (define prog (compile-progs (list p1 p2) ctx))
  (for ([pt points])
    (with-check-info (['point (map list fv pt)])
                     (match-define (vector v1 v2) (apply prog pt))
                     (check-equal? v1 v2))))

(define (check-rule rule)
  (check-rule-correct rule)
  (when (set-member? (rule-tags rule) 'sound)
    (check-rule-sound rule))
  (when (set-member? (rule-tags rule) 'fp-safe)
    (for ([rule* (rule->impl-rules rule)])
      (check-rule-fp-safe rule*))))

(module+ main
  (num-test-points (* 100 (num-test-points)))
  (command-line #:args names
                (for ([name names])
                  (eprintf "Checking ~a...\n" name)
                  (define test-rule
                    (first (filter (λ (x) (equal? (~a (rule-name x)) name)) (*rules*))))
                  (check-rule test-rule))))

(module+ test
  (for* ([rule (in-list (*rules*))])
    (test-case (~a (rule-name rule))
      (check-rule rule))))
