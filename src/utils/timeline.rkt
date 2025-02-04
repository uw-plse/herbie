#lang racket

(require json
         "../config.rkt"
         racket/hash)

(provide (rename-out [timeline-push! timeline-push!/unsafe] [timeline-start! timeline-start!/unsafe])
         (contract-out (timeline-event! (symbol? . -> . void?))
                       (timeline-push! (symbol? jsexpr? ... . -> . void?))
                       (timeline-adjust! (symbol? symbol? jsexpr? ... . -> . void?))
                       (timeline-start! (symbol? jsexpr? ... . -> . (-> void?))))
         timeline-load!
         timeline-extract
         timeline-merge
         timeline-relink
         *timeline-disabled*)
(module+ debug
  (provide *timeline*))

;; This is a box so we can get a reference outside the engine, and so
;; access its value even in a timeout.
;; Important: Use 'eq?' based hash tables, process may freeze otherwise
(define/reset *timeline* (box '()) (lambda () (set-box! (*timeline*) '())))

(define *timeline-active-key* #f)
(define *timeline-active-value* #f)

(define *timeline-disabled* (make-parameter true))

(define always-compact '(mixsample outcomes))

(define (timeline-event! type)
  (when *timeline-active-key*
    (hash-update! (car (unbox (*timeline*)))
                  *timeline-active-key*
                  (curry append *timeline-active-value*)
                  '())
    (set! *timeline-active-key* #f))

  (unless (*timeline-disabled*)
    (when (pair? (unbox (*timeline*)))
      (for ([key (in-list always-compact)]
            #:when (hash-has-key? (car (unbox (*timeline*))) key))
        (timeline-compact! key)))
    (define live-memory (current-memory-use #f))
    (define alloc-memory (current-memory-use 'cumulative))
    (define b
      (make-hasheq (list (cons 'type (~a type))
                         (cons 'time (current-inexact-milliseconds))
                         (cons 'gc-time (current-gc-milliseconds))
                         (cons 'memory (list (list live-memory alloc-memory))))))
    (set-box! (*timeline*) (cons b (unbox (*timeline*))))))

(define (timeline-push! key . values)
  (unless (*timeline-disabled*)
    (define val
      (if (null? (cdr values))
          (car values)
          values))
    (cond
      [(eq? *timeline-active-key* key)
       (set! *timeline-active-value* (cons val *timeline-active-value*))]
      [(not *timeline-active-key*)
       (unless (pair? (unbox (*timeline*)))
         (error 'timeline "Cannot push '~a to an empty timeline." key))
       (set! *timeline-active-key* key)
       (set! *timeline-active-value* (list val))]
      [else
       (when *timeline-active-key*
         (hash-update! (car (unbox (*timeline*)))
                       *timeline-active-key*
                       (curry append *timeline-active-value*)
                       '()))
       (set! *timeline-active-key* key)
       (set! *timeline-active-value* (list val))])))

(define (timeline-adjust! type key . values)
  (unless (*timeline-disabled*)
    (for/first ([cell (unbox (*timeline*))]
                #:when (equal? (hash-ref cell 'type) (~a type)))
      (hash-set! cell key values)
      true)
    (void)))

(define *timeline-1st-timer* #f)
(define *timeline-2nd-timer* #f)
(define *timeline-timers* (mutable-set))

(define (timeline-start! key . values)
  (define tstart (current-inexact-milliseconds))
  (cond
    [(not *timeline-1st-timer*)
     (define (end!)
       (define tend (current-inexact-milliseconds))
       (apply timeline-push! key (- tend tstart) values)
       (set! *timeline-1st-timer* #f))
     (set! *timeline-1st-timer* end!)
     end!]
    [(not *timeline-2nd-timer*)
     (define (end!)
       (define tend (current-inexact-milliseconds))
       (apply timeline-push! key (- tend tstart) values)
       (set! *timeline-2nd-timer* #f))
     (set! *timeline-2nd-timer* end!)
     end!]
    [*timeline-1st-timer* ; Slow path, more than one timer at a time
     (define (end!)
       (define tend (current-inexact-milliseconds))
       (apply timeline-push! key (- tend tstart) values)
       (set-remove! *timeline-timers* end!))
     (set-add! *timeline-timers* end!)
     end!]))

(define (timeline-load! value)
  (*timeline* value))

(define (diff-memory-records v1 v2)
  (list (list (- (caar v1) (caar v2)) (- (cadar v1) (cadar v2)))))

(define (timeline-extract)
  (when *timeline-1st-timer*
    (*timeline-1st-timer*))
  (when *timeline-2nd-timer*
    (*timeline-2nd-timer*))
  (when *timeline-active-key*
    (hash-update! (car (unbox (*timeline*)))
                  *timeline-active-key*
                  (curry append *timeline-active-value*)
                  '())
    (set! *timeline-active-key* #f))
  (for ([end! (set->list *timeline-timers*)])
    (end!))
  (define end
    (hasheq 'time
            (current-inexact-milliseconds)
            'gc-time
            (current-gc-milliseconds)
            'memory
            (list (list (current-memory-use #f) (current-memory-use 'cumulative)))))
  (reverse (for/list ([evt (unbox (*timeline*))]
                      [next (cons end (unbox (*timeline*)))])
             (define evt* (hash-copy evt))
             (hash-update! evt* 'time (λ (v) (- (hash-ref next 'time) v)))
             (hash-update! evt* 'gc-time (λ (v) (- (hash-ref next 'gc-time) v)))
             (hash-update! evt* 'memory (λ (v) (diff-memory-records (hash-ref next 'memory) v)))
             evt*)))

(define (timeline-relink link timeline)
  (for/list ([event (in-list timeline)])
    (for/hasheq ([(k v) (in-hash event)])
      (if (equal? k 'link)
          (values k (map (λ (p) (path->string (build-path link p))) v))
          (values k v)))))

(define timeline-types (make-hasheq))

(define-syntax define-timeline
  (syntax-rules ()
    [(_ name [field] ...) (hash-set! timeline-types 'name append)]
    [(_ name (field type) ...)
     (hash-set! timeline-types 'name (procedure-rename (make-merger type ...) 'name))]
    [(_ name #:custom fn) (hash-set! timeline-types 'name fn)]
    [(_ name #:unmergable) (hash-set! timeline-types 'name #f)]))

(define ((make-merger . fields) . tables)
  (define rows (apply append tables))
  (define groups (make-hash))
  (for ([row rows])
    (define-values (values key*) (partition cdr (map cons row fields)))
    (define key (map car key*))
    (if (hash-has-key? groups key)
        (hash-update! groups
                      key
                      (λ (old)
                        (for/list ([value2 old]
                                   [(value1 fn) (in-dict values)])
                          (fn value2 value1))))
        (hash-set! groups key (map car values))))
  (for/list ([(k v) (in-hash groups)])
    (let loop ([fields fields]
               [k k]
               [v v])
      (match* (fields k v)
        [((cons #f f*) (cons k k*) v) (cons k (loop f* k* v))]
        [((cons _ f*) k (cons v v*)) (cons v (loop f* k v*))]
        [('() '() '()) '()]))))

(define (merge-sampling-tables l1 l2)
  (let loop ([l1 (sort l1 < #:key first)]
             [l2 (sort l2 < #:key first)])
    (match-define (list n1 t1) (car l1))
    (match-define (list n2 t2) (car l2))
    (define rec (list n1 (hash-union t1 t2 #:combine +)))
    (match* ((cdr l1) (cdr l2))
      [('() '()) (list rec)]
      [('() l2*) (cons rec (loop (list (list (+ n1 1) t1)) l2*))]
      [(l1* '()) (cons rec (loop l1* (list (list (+ n2 1) t2))))]
      [(l1* l2*) (cons rec (loop l1* l2*))])))

(define-timeline type #:custom (λ (a b) a))
(define-timeline time #:custom +)
(define-timeline gc-time #:custom +)

(define-timeline memory [live +] [alloc +])
(define-timeline method [method])
(define-timeline mixsample [time +] [function false] [precision false])
(define-timeline rules [rule false] [count +])
(define-timeline times [time +] [input false])
(define-timeline series [time +] [expr false] [var false] [transform false])
(define-timeline compiler [before +] [after +])
(define-timeline outcomes [time +] [prec false] [category false] [count +])
(define-timeline accuracy [accuracy])
(define-timeline oracle [oracle])
(define-timeline baseline [baseline])
(define-timeline count [input +] [output +])
(define-timeline alts #:unmergable)
(define-timeline inputs #:unmergable)
(define-timeline outputs #:unmergable)
(define-timeline sampling #:custom merge-sampling-tables)
(define-timeline bogosity #:custom (λ (x y) (list (hash-union (car x) (car y) #:combine +))))
(define-timeline symmetry #:unmergable)
(define-timeline remove-preprocessing #:unmergable)
(define-timeline locations #:unmergable)
(define-timeline bstep #:unmergable)
(define-timeline kept #:unmergable)
(define-timeline min-error #:unmergable)
(define-timeline egraph #:unmergable)
(define-timeline stop [reason false] [count +])
(define-timeline branch #:unmergable)
(define-timeline explanations
                 [op false]
                 [expr (const #f)]
                 [expl false]
                 [count +]
                 [mcount +]
                 [flows (const #f)]
                 [locations (const #f)])
(define-timeline confusion #:custom (λ (x y) (list (map + (car x) (car y)))))
(define-timeline total-confusion #:custom (λ (x y) (list (map + (car x) (car y)))))
(define-timeline maybe-confusion #:custom (λ (x y) (list (map + (car x) (car y)))))
(define-timeline freqs [key false] [val +])

(define (timeline-merge . timelines)
  ;; The timelines in this case are JSON objects, as above
  (define types (make-hash))
  (for* ([tl (in-list timelines)]
         [event tl])
    (define data (hash-ref! types (hash-ref event 'type) (make-hash)))
    (for ([(k v) (in-dict event)]
          #:when (hash-ref timeline-types k #f))
      (if (hash-has-key? data k)
          (hash-update! data k (λ (old) ((hash-ref timeline-types k) v old)))
          (hash-set! data k v))))

  (sort (hash-values types) > #:key (curryr hash-ref 'time)))

(define (timeline-compact! key)
  (unless (*timeline-disabled*)
    (define fn (hash-ref timeline-types key #f))
    (when fn
      (hash-update! (car (unbox (*timeline*))) key (curryr fn '()) '()))))
