#lang racket

(provide (struct-out pareto-point)
         pareto-map
         pareto-union
         pareto-minimize
         pareto-convex
         pareto-combine)

(struct pareto-point (cost error data) #:prefab)

(define (ppt->pt ppt)
  (list (pareto-point-cost ppt) (pareto-point-error ppt)))

(define (pt->ppt pt)
  (pareto-point (first pt) (second pt) (list)))

(define (pareto-shift ppt0 frontier)
  (match-define (pareto-point cost0 err0 _) ppt0)
  (for/list ([ppt (in-list frontier)])
    (match-define (pareto-point cost err _) ppt)
    (pareto-point (+ cost0 cost) (+ err0 err) (list))))

(define (pareto-compare pt1 pt2)
  (match-define (pareto-point cost1 err1 data1) pt1)
  (match-define (pareto-point cost2 err2 data2) pt2)
  (cond
    [(and (= cost1 cost2) (= err1 err2)) '=]
    [(and (<= cost1 cost2) (<= err1 err2)) '<]
    [(and (>= cost1 cost2) (>= err1 err2)) '>]
    [else '<>]))

(define (pareto-map f curve)
  (for/list ([ppt (in-list curve)])
    (struct-copy pareto-point ppt [data (f (pareto-point-data ppt))])))

;; Takes two lists of `pareto-point` structs that are Pareto-optimal
;; and returns the Pareto-optimal subset of their union.
;; The curves most be sorted using the same method.
(define (pareto-union curve1 curve2 #:combine [combine (lambda (a b) (append a b))])
  (let loop ([curve1 curve1]
             [curve2 curve2])
    ; The curve is sorted so that highest accuracy is first
    (match* (curve1 curve2)
      [('() _) curve2]
      [(_ '()) curve1]
      [((cons ppt1 rest1) (cons ppt2 rest2))
       (match (pareto-compare ppt1 ppt2)
         ['< (loop curve1 rest2)]
         ['> (loop rest1 curve2)]
         ['=
          (define joint-data (combine (pareto-point-data ppt1) (pareto-point-data ppt2)))
          (define joint (struct-copy pareto-point ppt1 [data joint-data]))
          (cons joint (loop rest1 rest2))]
         ['<>
          (if (< (pareto-point-error ppt1) (pareto-point-error ppt2))
              (cons ppt1 (loop rest1 curve2))
              (cons ppt2 (loop curve1 rest2)))])])))

;; Takes a Pareto frontier and returns the subset of
;; points that are convex.
(define (pareto-convex ppts)
  (let loop ([ppts* '()]
             [ppts ppts])
    (match ppts
      [(list p0 p1 p2 pns ...)
       (match-define (pareto-point p0x p0y _) p0)
       (match-define (pareto-point p1x p1y _) p1)
       (match-define (pareto-point p2x p2y _) p2)
       ; if { p0, p1, p2 } are not convex:
       ;   discard p1
       ;   try backtracking one point (if not continue)
       ; else move forward one point
       (define m01 (/ (- p1y p0y) (- p1x p0x)))
       (define m12 (/ (- p2y p1y) (- p2x p1x)))
       (match* ((> m12 m01) (null? ppts*))
         [(#t #t) (loop ppts* (append (list p0 p2) pns))]
         [(#t #f) (loop (rest ppts*) (append (list (first ppts*) p0 p2) pns))]
         [(#f _) (loop (cons p0 ppts*) (append (list p1 p2) pns))])]
      [_ (append (reverse ppts*) ppts)])))

;; Takes a list of `pareto-point` structs
;; and returns the Pareto-optimal subset.
(define (pareto-minimize ppts)
  (define ppts* (sort ppts < #:key pareto-point-cost))
  (for/fold ([minimized '()]) ([ppt (in-list ppts*)])
    (pareto-union (list ppt) minimized)))

;; Creates a synthetic frontier from multiple frontiers
;; as described in the ARITH '21 paper.
(define (pareto-combine frontiers #:convex? [convex? #f])
  (define (finalize f)
    (if convex?
        (pareto-convex f)
        f))
  (define frontiers* (map (λ (f) (pareto-minimize (map pt->ppt f))) frontiers))
  (for/fold ([combined (list)]
             #:result (map ppt->pt combined))
            ([frontier (in-list frontiers*)])
    (if (null? combined)
        (finalize frontier)
        (for/fold ([combined* (list)]
                   #:result (finalize combined*))
                  ([ppt (in-list combined)])
          (define ppts (pareto-minimize (pareto-shift ppt frontier)))
          (pareto-union ppts combined*)))))

(module+ test
  (require rackunit)

  (define (make-pareto pts)
    (sort (for/list ([pt (in-list pts)])
            (match-define (list cost err altns ...) pt)
            (pareto-point cost err altns))
          <
          #:key pareto-point-error))

  (define (from-pareto pts)
    (sort (for/list ([ppt (in-list pts)])
            (match-define (pareto-point cost err altns) ppt)
            (list* cost err altns))
          <
          #:key first))

  (define (pareto-add curve d c e)
    (pareto-union (list (pareto-point c e (list d))) curve))

  (check-equal? (from-pareto (make-pareto '((1 5 a) (2 3 b) (5 1 a b)))) '((1 5 a) (2 3 b) (5 1 a b)))
  (check-equal? (from-pareto (pareto-add (make-pareto '()) 'a 1 5)) '((1 5 a)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (5 1 b))) 'c 3 3))
                '((1 5 a) (3 3 c) (5 1 b)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b))) 'c 5 1))
                '((1 5 a) (3 3 b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((3 3 b) (5 1 c))) 'a 1 5))
                '((1 5 a) (3 3 b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b) (5 1 c))) 'd 1 5))
                '((1 5 d a) (3 3 b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b) (5 1 c))) 'd 3 3))
                '((1 5 a) (3 3 d b) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 5 a) (3 3 b) (5 1 c))) 'd 2 2))
                '((1 5 a) (2 2 d) (5 1 c)))
  (check-equal? (from-pareto (pareto-add (make-pareto '((1 1 a))) 'b 1 3)) '((1 1 a))))
