#lang racket

(require json
         racket/date)
(require "../syntax/types.rkt"
         "../utils/common.rkt"
         "../utils/pareto.rkt")

(provide (struct-out table-row)
         (struct-out report-info)
         make-report-info
         read-datafile
         write-datafile
         merge-datafiles
         diff-datafiles)

(struct table-row
        (name identifier
              status
              pre
              preprocess
              precision
              conversions
              vars
              warnings
              input
              output
              spec
              target-prog
              start
              result
              target
              start-est
              result-est
              time
              link
              cost-accuracy)
  #:prefab)

(struct report-info
        (date commit branch hostname seed flags points iterations note tests merged-cost-accuracy)
  #:prefab
  #:mutable)

(define (make-report-info tests #:note [note ""] #:seed [seed #f])
  (report-info (current-date)
               *herbie-commit*
               *herbie-branch*
               *hostname*
               (or seed (get-seed))
               (*flags*)
               (*num-points*)
               (*num-iterations*)
               note
               tests
               (merged-cost-accuracy tests)))

;; Calculate the initial cost and accuracy and the rescaled and combined Pareto
;; frontier for the given `tests` and return these as a list.
(define (merged-cost-accuracy tests)
  (define tests-length (length tests))
  (define cost-accuracies (map table-row-cost-accuracy tests))
  (define maximum-accuracy
    (for/sum ([test (in-list tests)])
             (representation-total-bits (get-representation (table-row-precision test)))))
  (define initial-accuracy
    (let ([initial-accuracies-sum
           (for/sum ([cost-accuracy (in-list cost-accuracies)] #:unless (null? cost-accuracy))
                    (match cost-accuracy
                      [(list (list _ initial-accuracy) _ _) initial-accuracy]))])
      (if (> maximum-accuracy 0)
          (exact->inexact (- 1 (/ initial-accuracies-sum maximum-accuracy)))
          1.0)))
  (define rescaled
    (for/list ([cost-accuracy (in-list cost-accuracies)]
               #:unless (null? cost-accuracy))
      (match-define (list (and initial-point (list initial-cost _)) best-point other-points)
        cost-accuracy)
      ;; Has to be floating point so serializing to JSON doesn't complain
      ;; about rational numbers later
      (define initial-cost* (exact->inexact initial-cost))
      (for/list ([point (in-list (list* initial-point best-point other-points))])
        (match-define (list cost accuracy _ ...) point)
        (list (/ cost initial-cost*) accuracy))))
  (define frontier
    (map
     (match-lambda
       ;; Equivalent to (/ 1 (/ cost tests-length))
       [(list cost accuracy) (list (/ 1 (/ cost tests-length)) (- 1 (/ accuracy maximum-accuracy)))])
     (pareto-combine rescaled #:convex? #t)))
  (define maximum-cost
    (argmax identity
            (cons 0.0 ;; To prevent `argmax` from signaling an error in case `tests` is empty
                  (map (match-lambda
                         [(list cost _) cost])
                       frontier))))
  (list (list 1.0 initial-accuracy) frontier))

(define (write-datafile file info)
  (define (simplify-test test)
    (match test
      [(table-row name
                  identifier
                  status
                  pre
                  preprocess
                  prec
                  conversions
                  vars
                  warnings
                  input
                  output
                  spec
                  target-prog
                  start-bits
                  end-bits
                  target-bits
                  start-est
                  end-est
                  time
                  link
                  cost-accuracy)
       (define bits (representation-total-bits (get-representation prec)))
       (define cost-accuracy*
         (match cost-accuracy
           [(list) (list)]
           [(list start best others)
            (list start
                  best
                  (for/list ([other (in-list others)])
                    (match-define (list cost error expr) other)
                    (list cost error (~a expr))))]))
       (make-hash `((name . ,name) (identifier . ,(~s identifier))
                                   (pre . ,(~s pre))
                                   (preprocess . ,(~s preprocess))
                                   (prec . ,(~s prec))
                                   (bits . ,(representation-total-bits (get-representation prec)))
                                   (conversions . ,(map (curry map ~s) conversions))
                                   (status . ,status)
                                   (start . ,start-bits)
                                   (end . ,end-bits)
                                   (target . ,target-bits)
                                   (start-est . ,start-est)
                                   (end-est . ,end-est)
                                   (vars . ,(if vars
                                                (map symbol->string vars)
                                                #f))
                                   (warnings . ,(map ~s warnings))
                                   (input . ,(~s input))
                                   (output . ,(~s output))
                                   (spec . ,(~s spec))
                                   (target-prog . ,(~s target-prog))
                                   (time . ,time)
                                   (link . ,(~a link))
                                   (cost-accuracy . ,cost-accuracy*)))]))

  (define data
    (match info
      [(report-info date
                    commit
                    branch
                    hostname
                    seed
                    flags
                    points
                    iterations
                    note
                    tests
                    merged-cost-accuracy)
       (make-hash `((date . ,(date->seconds date)) (commit . ,commit)
                                                   (branch . ,branch)
                                                   (hostname . ,hostname)
                                                   (seed . ,(~a seed))
                                                   (flags . ,(flags->list flags))
                                                   (points . ,points)
                                                   (iterations . ,iterations)
                                                   (note . ,note)
                                                   (tests . ,(map simplify-test tests))
                                                   (merged-cost-accuracy . ,merged-cost-accuracy)))]))

  (if (port? file)
      (write-json data file)
      (call-with-atomic-output-file file (λ (p name) (write-json data p)))))

(define (flags->list flags)
  (for*/list ([rec (hash->list flags)]
              [fl (cdr rec)])
    (format "~a:~a" (car rec) fl)))

(define (list->flags list)
  (make-hash (for/list ([part (group-by car
                                        (map (compose (curry map string->symbol)
                                                      (curryr string-split ":"))
                                             list))])
               (cons (car (first part)) (map cadr part)))))

(define (read-datafile port)
  (define (parse-string s)
    (if s
        (call-with-input-string s read)
        #f))

  (let* ([json (read-json port)]
         [get (λ (field) (hash-ref json field))])
    (report-info (seconds->date (get 'date))
                 (get 'commit)
                 (get 'branch)
                 (hash-ref json 'hostname "")
                 (parse-string (get 'seed))
                 (list->flags (get 'flags))
                 (get 'points)
                 (get 'iterations)
                 (hash-ref json 'note #f)
                 (for/list ([test (get 'tests)]
                            #:when (hash-has-key? test 'vars))
                   (let ([get (λ (field) (hash-ref test field))])
                     (define vars
                       (match (hash-ref test 'vars)
                         [(list names ...) (map string->symbol names)]
                         [string-lst (parse-string string-lst)]))
                     (define cost-accuracy
                       (match (hash-ref test 'cost-accuracy '())
                         [(list) (list)]
                         [(list start best others)
                          (list start
                                best
                                (for/list ([other (in-list others)])
                                  (match-define (list cost err expr) other)
                                  (list cost err (parse-string expr))))]
                         [(? string? s) (parse-string s)]))
                     (table-row (get 'name)
                                (parse-string (hash-ref test 'identifier "#f"))
                                (get 'status)
                                (parse-string (hash-ref test 'pre "TRUE"))
                                (parse-string (hash-ref test 'preprocess "()"))
                                (parse-string (hash-ref test 'prec "binary64"))
                                (let ([cs (hash-ref test 'conversions "()")])
                                  (if (string? cs)
                                      (parse-string cs)
                                      (map (curry map parse-string) cs)))
                                vars
                                (map string->symbol (hash-ref test 'warnings '()))
                                (parse-string (get 'input))
                                (parse-string (get 'output))
                                (parse-string (hash-ref test 'spec "#f"))
                                (parse-string (hash-ref test 'target-prog "#f"))
                                (get 'start)
                                (get 'end)
                                (get 'target)
                                (hash-ref test 'start-est 0)
                                (hash-ref test 'end-est 0)
                                (get 'time)
                                (get 'link)
                                cost-accuracy)))
                 (hash-ref json 'merged-cost-accuracy null))))

(define (unique? a)
  (or (null? a) (andmap (curry equal? (car a)) (cdr a))))

(define (merge-datafiles dfs #:dirs [dirs #f] #:name [name #f])
  (when (null? dfs)
    (error 'merge-datafiles "Cannot merge no datafiles"))
  (for ([f (in-list (list report-info-commit
                          report-info-hostname
                          report-info-seed
                          report-info-flags
                          report-info-points
                          report-info-iterations))])
    (unless (unique? (map f dfs))
      (error 'merge-datafiles "Cannot merge datafiles at different ~a" f)))
  (unless dirs
    (set! dirs (map (const #f) dfs)))
  (define tests
    (for/list ([df (in-list dfs)]
               [dir (in-list dirs)]
               #:when true
               [test (in-list (report-info-tests df))])
      (struct-copy table-row
                   test
                   (link (if dir
                             (format "~a/~a" dir (table-row-link test))
                             (table-row-link test))))))

  (report-info (last (sort (map report-info-date dfs) < #:key date->seconds))
               (report-info-commit (first dfs))
               (first (filter values (map report-info-branch dfs)))
               (report-info-hostname (first dfs))
               (report-info-seed (first dfs))
               (report-info-flags (first dfs))
               (report-info-points (first dfs))
               (report-info-iterations (first dfs))
               (if name
                   (~a name)
                   (~a (cons 'merged (map report-info-note dfs))))
               tests
               ;; Easiest to just recompute everything based off the combined tests
               (merged-cost-accuracy tests)))

(define (diff-datafiles old new)
  (define old-tests
    (for/hash ([ot (in-list (report-info-tests old))])
      (values (table-row-name ot) ot)))
  (define tests*
    (for/list ([nt (in-list (report-info-tests new))])
      (if (hash-has-key? old-tests (table-row-name nt))
          (let ([ot (hash-ref old-tests (table-row-name nt))])
            (define end-score (table-row-result nt))
            (define target-score (table-row-result ot))
            (define start-score (table-row-start nt))

            (struct-copy table-row
                         nt
                         [status
                          (if (and end-score target-score start-score)
                              (cond
                                [(< end-score (- target-score 1)) "gt-target"]
                                [(< end-score (+ target-score 1)) "eq-target"]
                                [(> end-score (+ start-score 1)) "lt-start"]
                                [(> end-score (- start-score 1)) "eq-start"]
                                [(> end-score (+ target-score 1)) "lt-target"])
                              (table-row-status nt))]
                         [target-prog (table-row-output ot)]
                         [target (table-row-result ot)]))
          nt)))
  (struct-copy report-info new [tests tests*]))
