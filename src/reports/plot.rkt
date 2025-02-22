#lang racket

(require math/bigfloat
         math/flonum)
(require "../core/points.rkt"
         "../utils/float.rkt"
         "../core/programs.rkt"
         "../syntax/types.rkt"
         "../syntax/read.rkt"
         "../utils/alternative.rkt"
         "../core/bsearch.rkt"
         "../api/sandbox.rkt")

(provide make-points-json
         regime-var
         regime-splitpoints
         real->ordinal
         splitpoints->json)

(define (all-same? pts idx)
  (= 1
     (set-count (for/set ([pt pts])
                  (list-ref pt idx)))))

(define (ulps->bits-tenths x)
  (string->number (real->decimal-string (ulps->bits x) 1)))

(define (splitpoints->json vars alt repr)
  (for/list ([var (in-list vars)])
    (define split-var? (equal? var (regime-var alt)))
    (if split-var?
        (for/list ([val (regime-splitpoints alt)])
          (real->ordinal (repr->real val repr) repr))
        '())))

(define (make-points-json result-hash)
  (define test (hash-ref result-hash 'test))
  (define backend (hash-ref result-hash 'backend))
  (define pctxs (hash-ref backend 'pctxs))
  (define start (hash-ref backend 'start))
  (define targets (hash-ref backend 'target))
  (define end (hash-ref backend 'end))

  (define repr (test-output-repr test))
  (define start-errors (alt-analysis-test-errors start))

  (define target-errors (map alt-analysis-test-errors targets))

  (define end-errors (hash-ref end 'end-errors))

  (define newpoints (pcontext-points (second pctxs)))

  ; Immediately convert points to reals to handle posits
  (define points
    (for/list ([point newpoints])
      (for/list ([x point])
        (repr->real x repr))))

  (define json-points
    (for/list ([point points])
      (for/list ([value point])
        (real->ordinal value repr))))

  (define vars (test-vars test))
  (define bits (representation-total-bits repr))
  (define start-error (map ulps->bits-tenths start-errors))
  (define target-error (map (lambda (alt-error) (map ulps->bits-tenths alt-error)) target-errors))
  (define end-error (map ulps->bits-tenths (car end-errors)))

  (define target-error-entries
    (for/list ([i (in-naturals)]
               [error-value (in-list target-error)])
      (cons (format "target~a" (+ i 1)) error-value)))

  (define error-entries
    (list* (cons "start" start-error) (cons "end" end-error) target-error-entries))

  (define ticks
    (for/list ([var (in-list vars)]
               [idx (in-naturals)]
               #:unless (all-same? newpoints idx))
      ; We want to bail out since choose-ticks will crash otherwise
      (define points-at-idx (map (curryr list-ref idx) points))
      (define real-ticks (choose-ticks (apply min points-at-idx) (apply max points-at-idx) repr))
      (for/list ([val real-ticks])
        (define tick-str
          (if (or (= val 0) (< 0.01 (abs val) 100))
              (~r (exact->inexact val) #:precision 4)
              (string-replace (~r val #:notation 'exponential #:precision 0) "1e" "e")))
        (list tick-str (real->ordinal val repr)))))

  (define splitpoints (hash-ref end 'splitpoints))

  ; NOTE ordinals *should* be passed as strings so we can detect truncation if
  ;   necessary, but this isn't implemented yet.
  ; Fields:
  ;   bits: int representing the maximum possible bits of error
  ;   vars: array of n string variable names
  ;   points: array of size m like [[x0, x1, ..., xn], ...] where x0 etc.
  ;     are ordinals representing the real input values
  ;   error: JSON dictionary where keys are {start, end, target1, ..., targetn}.
  ;          Each key's value holds an array like [y0, ..., ym] where y0 etc are
  ;          bits of error for the output on each point
  ;   ticks: array of size n where each entry is 13 or so tick values as [ordinal, string] pairs
  ;   splitpoints: array with the ordinal splitpoints
  `#hasheq((bits . ,bits)
           (vars . ,(map symbol->string vars))
           (points . ,json-points)
           (error . ,error-entries)
           (ticks_by_varidx . ,ticks)
           (splitpoints_by_varidx . ,splitpoints)))
;;  Repr conversions

(define (ordinal->real x repr)
  (repr->real ((representation-ordinal->repr repr) x) repr))

(define (real->ordinal x repr)
  ((representation-repr->ordinal repr) (real->repr x repr)))

(define (first-power10 min max repr)
  (define value
    (cond
      [(negative? max) (- (expt 10 (ceiling (/ (log (- max)) (log 10)))))]
      [else (expt 10 (floor (/ (log max) (log 10))))]))
  (if (<= value min) #f value))

(define (clamp x lo hi)
  (min hi (max x lo)))

(define (choose-between min max number repr)
  ; Returns a given number of ticks, roughly evenly spaced, between min and max
  ; For any tick, n divisions below max, the tick is an ordinal corresponding to:
  ;  (a) a power of 10 between n and (n + ε) divisions below max where ε is some tolerance, or
  ;  (b) a value, n divisions below max
  (define sub-range (round (/ (- max min) (add1 number))))
  (define (near x n)
    (and (<= x n) (<= (abs (/ (- x n) sub-range)) 0.2))) ; <- tolerance
  (for/list ([itr (in-range 1 (add1 number))])
    (define power10
      (first-power10 (ordinal->real (clamp (- max (* (add1 itr) sub-range)) min max) repr)
                     (ordinal->real (clamp (- max (* itr sub-range)) min max) repr)
                     repr))
    (if (and power10 (near (real->ordinal power10 repr) (- max (* itr sub-range))))
        (real->ordinal power10 repr)
        (- max (* itr sub-range)))))

(define (pick-spaced-ordinals necessary min max number repr)
  (define sub-range (/ (- max min) number)) ; size of a division on the ordinal range
  (define necessary* ; filter out necessary points that are too close
    (let loop ([necessary necessary])
      (cond
        [(< (length necessary) 2) necessary]
        [(< (- (cadr necessary) (car necessary)) sub-range) (loop (cdr necessary))]
        [else (cons (car necessary) (loop (cdr necessary)))])))
  (define all
    (let loop ([necessary necessary*]
               [min* min]
               [start 0])
      (cond
        [(>= start number) '()]
        [(empty? necessary) (choose-between min* max (- number start) repr)]
        [else
         (define idx
           (for/first ([i (in-range number)]
                       #:when (<= (- (first necessary) (+ min (* i sub-range))) sub-range))
             i))
         (append (choose-between min* (first necessary) (- idx start) repr)
                 (loop (cdr necessary) (first necessary) (add1 idx)))])))
  (sort (append all necessary*) <))

(define (choose-ticks min max repr)
  (define tick-count 13)
  (define necessary
    (map (curryr real->ordinal repr) (filter (λ (x) (<= min x max)) (list min -1.0 0.0 1.0 max))))
  (map (curryr ordinal->real repr)
       (pick-spaced-ordinals necessary
                             (real->ordinal min repr)
                             (real->ordinal max repr)
                             tick-count
                             repr)))

(define/contract (regime-info altn)
  (-> alt? (or/c (listof sp?) #f))
  (let loop ([altn altn])
    (match altn
      [(alt _ `(regimes ,splitpoints) prevs _) splitpoints]
      [(alt _ _ (list) _) #f]
      [(alt _ _ (list prev _ ...) _) (loop prev)])))

(define (regime-splitpoints altn)
  (map sp-point (drop-right (regime-info altn) 1)))

(define/contract (regime-var altn)
  (-> alt? (or/c expr? #f))
  (define info (regime-info altn))
  (and info (sp-bexpr (car info))))
