#lang racket

(require "../utils/common.rkt"
         "programs.rkt"
         "../utils/timeline.rkt"
         "../utils/errors.rkt"
         "rules.rkt"
         "../utils/alternative.rkt"
         "egg-herbie.rkt"
         "batch.rkt")

(provide (contract-out [simplify-batch (-> egg-runner? batch? (listof (listof batchref?)))]))

(module+ test
  (require rackunit
           "../syntax/load-plugin.rkt")
  (load-herbie-plugins))

(define (simplify-batch runner batch)
  (timeline-push! 'inputs (map ~a (batch->progs (egg-runner-batch runner) (egg-runner-roots runner))))

  (define simplifieds (egraph-best runner batch))
  (define out
    (for/list ([simplified (in-list simplifieds)]
               [root (egg-runner-roots runner)])
      (remove-duplicates (cons (batchref (egg-runner-batch runner) root) simplified)
                         #:key batchref-idx)))

  (timeline-push! 'outputs (map (compose ~a debatchref) (apply append out)))
  out)

(module+ test
  (require "../syntax/types.rkt"
           "rules.rkt")

  ;; set parameters
  (define vars '(x a b c))
  (*context* (make-debug-context vars))

  (define (test-simplify . args)
    (define batch (progs->batch args))
    (define runner
      (make-egraph batch
                   (batch-roots batch)
                   (map (lambda (_) 'real) args)
                   `((,(*simplify-rules*) . ((node . ,(*node-limit*)))))))
    (parameterize ([*egraph-platform-cost* #f])
      (map (compose debatchref last) (simplify-batch runner batch))))

  (define test-exprs
    '((1 . 1) (0 . 0)
              ((+ 1 0) . 1)
              ((+ 1 5) . 6)
              ((+ x 0) . x)
              ((- x 0) . x)
              ((* x 1) . x)
              ((/ x 1) . x)
              ((- (* 1 x) (* (+ x 1) 1)) . -1)
              ((- (+ x 1) x) . 1)
              ((- (+ x 1) 1) . x)
              ((/ (* x 3) x) . 3)
              ((- (* (sqrt (+ x 1)) (sqrt (+ x 1))) (* (sqrt x) (sqrt x))) . 1)
              ((+ 1/5 3/10) . 1/2)
              ((cos (PI)) . -1)
              ((pow (E) 1) . (E))
              ;; this test is problematic and runs out of nodes currently
              ;;((/ 1 (- (/ (+ 1 (sqrt 5)) 2) (/ (- 1 (sqrt 5)) 2))) . (/ 1 (sqrt 5)))
              ))

  (*timeline-disabled* true)
  (define outputs (apply test-simplify (dict-keys test-exprs)))
  (for ([(original target) (in-dict test-exprs)]
        [output outputs])
    (with-check-info (['original original]) (check-equal? output target)))

  (check set-member? '((* x 6) (* 6 x)) (first (test-simplify '(+ (+ (+ (+ (+ x x) x) x) x) x))))

  (define no-crash-exprs
    '((exp (/ (/ (* (* c a) 4) (- (neg b) (sqrt (- (* b b) (* 4 (* a c)))))) (* 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr]) (check-not-exn (λ () (test-simplify expr))))))
