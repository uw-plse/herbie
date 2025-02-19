#lang racket

(require math/bigfloat
         rival)

(require "../syntax/types.rkt"
         "../core/rival.rkt"
         "../utils/timeline.rkt"
         "../utils/errors.rkt"
         "../utils/pretty-print.rkt"
         "../utils/float.rkt")

(provide find-intervals
         hyperrect-weight)

(struct search-space (true false other true-hints other-hints))

(define (make-search-space . ranges)
  (search-space '() '() ranges '() (make-list (length ranges) #f)))

(define (total-weight reprs)
  (expt 2 (apply + (map representation-total-bits reprs))))

(define (hyperrect-weight hyperrect reprs)
  (apply *
         (for/list ([interval (in-list hyperrect)]
                    [repr (in-list reprs)])
           (define ->ordinal
             (compose (representation-repr->ordinal repr) (representation-bf->repr repr)))
           (+ 1 (- (->ordinal (ival-hi interval)) (->ordinal (ival-lo interval)))))))

(define (search-step compiler space split-var)
  (define vars (real-compiler-vars compiler))
  (define reprs (real-compiler-var-reprs compiler))
  (match-define (search-space true false other true-hints other-hints) space)
  (define-values (true* false* other* true-hints* other-hints*)
    (for/fold ([true* true]
               [false* false]
               [other* '()]
               [true-hints* true-hints]
               [other-hints* '()])
              ([rect (in-list other)]
               [hint (in-list other-hints)])
      (match-define (list (ival err err?) hint* converged?)
        (real-compiler-analyze compiler (list->vector rect) hint))
      (when (eq? err 'unsamplable)
        (warn 'ground-truth
              #:url "faq.html#ground-truth"
              "could not determine a ground truth"
              #:extra (for/list ([var vars]
                                 [repr reprs]
                                 [ival rect])
                        (define val
                          (value->string ((representation-bf->repr repr)
                                          (bigfloat-pick-point (ival-lo ival) (ival-hi ival)))
                                         repr))
                        (format "~a = ~a" var val))))
      (cond
        [err (values true* (cons rect false*) other* true-hints* other-hints*)]
        [(and (not err?) converged?)
         (values (cons rect true*) false* other* (cons hint* true-hints*) other-hints*)]
        [else
         (define range (list-ref rect split-var))
         (define repr (list-ref reprs split-var))
         (match (two-midpoints repr (ival-lo range) (ival-hi range))
           [(cons midleft midright)
            (define rect-lo (list-set rect split-var (ival (ival-lo range) midleft)))
            (define rect-hi (list-set rect split-var (ival midright (ival-hi range))))
            (values true*
                    false*
                    (list* rect-lo rect-hi other*)
                    true-hints*
                    (list* hint* hint* other-hints*))]
           [#f (values true* false* (cons rect other*) true-hints* (cons hint* other-hints*))])])))
  (search-space true* false* other* true-hints* other-hints*))

(define (make-sampling-table reprs true false other)
  (define denom (total-weight reprs))
  (define true-weight (apply + (map (curryr hyperrect-weight reprs) true)))
  (define false-weight (apply + (map (curryr hyperrect-weight reprs) false)))
  (define other-weight (apply + (map (curryr hyperrect-weight reprs) other)))
  (define out (make-hash))
  (hash-set! out 'valid (exact->inexact (/ true-weight denom)))
  (hash-set! out 'unknown (exact->inexact (/ other-weight denom)))
  (hash-set! out 'invalid (exact->inexact (/ false-weight denom)))
  (define total (apply + (hash-values out)))
  (hash-update! out 'precondition (curry + (- 1 total)) 0)
  (make-immutable-hash (hash->list out)))

(define (find-intervals compiler rects #:fuel [depth 128])
  (define var-reprs (real-compiler-var-reprs compiler))
  (if (or (null? rects) (null? (first rects)))
      (map (curryr cons 'other) rects)
      (let loop ([space (apply make-search-space rects)]
                 [n 0])
        (match-define (search-space true false other true-hints other-hints) space)
        (timeline-push! 'sampling n (make-sampling-table var-reprs true false other))

        (define n* (remainder n (length (first rects))))
        (if (or (>= n depth) (empty? (search-space-other space)) (>= (length other) (expt 2 depth)))
            (list (append (search-space-true space) (search-space-other space))
                  (append (search-space-true-hints space) (search-space-other-hints space))
                  (make-sampling-table var-reprs true false other))
            (loop (search-step compiler space n*) (+ n 1))))))
