#lang racket
(require json
         (only-in xml write-xexpr xexpr?)
         racket/date)
(require "../utils/common.rkt"
         "../api/datafile.rkt"
         "common.rkt"
         "../syntax/types.rkt"
         "../utils/float.rkt"
         "../config.rkt")
(provide make-timeline)

(define timeline-phase? (hash/c symbol? any/c))
(define timeline? (listof timeline-phase?))

;; This first part handles timelines for a single Herbie run

(define (make-timeline name timeline #:info [info #f] #:path [path "."])
  `(html (head (meta ([charset "utf-8"]))
               (title "Metrics for " ,(~a name))
               (link ([rel "stylesheet"] [type "text/css"]
                                         [href ,(if info "report.css" "../report.css")]))
               (script ([src ,(if info "report.js" "../report.js")])))
         (body ,(render-menu (~a name)
                             #:path path
                             (if info
                                 `(("Report" . "index.html"))
                                 `(("Details" . "graph.html"))))
               ,(if info
                    (render-about info)
                    "")
               ,(render-timeline timeline)
               ,(render-profile))))

(define/contract (render-timeline timeline)
  (-> timeline? xexpr?)
  (define time (apply + (map (curryr dict-ref 'time) timeline)))
  `(section
    ([id "process-info"])
    (p ((class "header")) "Time bar (total: " (span ((class "number")) ,(format-time time)) ")")
    (div ((class "timeline"))
         ,@(for/list ([n (in-naturals)]
                      [curr timeline])
             `(div ((class ,(format "timeline-phase timeline-~a" (dict-ref curr 'type)))
                    [data-id ,(format "timeline~a" n)]
                    [data-type ,(~a (dict-ref curr 'type))]
                    [data-timespan ,(~a (dict-ref curr 'time))]))))
    ,@(for/list ([phase timeline]
                 [n (in-naturals)])
        (render-phase phase n time))))

(define/contract (render-phase curr n total-time)
  (-> timeline-phase? integer? real? xexpr?)
  (match-define (dict 'time time 'type type) curr)

  `(div ((class ,(format "timeline-block timeline-~a" type)) [id ,(format "timeline~a" n)])
        (h3 (a ([href ,(format "#timeline~a" n)]) ,(~a type))
            (span ((class "time")) ,(format-time time) " (" ,(format-percent time total-time) ")"))
        (dl ,@(dict-call curr render-phase-memory 'memory)
            ,@(dict-call curr render-phase-algorithm 'method)
            ,@(dict-call curr render-phase-locations 'locations)
            ,@(dict-call curr render-phase-accuracy 'accuracy 'oracle 'baseline 'name 'link 'repr)
            ,@(dict-call curr render-phase-pruning 'kept)
            ,@(dict-call curr render-phase-error 'min-error)
            ,@(dict-call curr render-phase-rules 'rules)
            ,@(dict-call curr render-phase-fperrors 'fperrors)
            ,@(dict-call curr render-phase-explanations 'explanations)
            ,@(dict-call curr render-phase-confusion 'confusion)
            ,@(dict-call curr render-phase-maybe-confusion 'maybe-confusion)
            ,@(dict-call curr render-phase-freqs 'freqs)
            ,@(dict-call curr render-phase-total-confusion 'total-confusion)
            ,@(dict-call curr render-phase-egraph 'egraph)
            ,@(dict-call curr render-phase-stop 'stop)
            ,@(dict-call curr render-phase-counts 'count)
            ,@(dict-call curr render-phase-alts 'alts)
            ,@(dict-call curr render-phase-inputs 'inputs 'outputs)
            ,@(dict-call curr render-phase-times #:extra n 'times)
            ,@(dict-call curr render-phase-series #:extra n 'series)
            ,@(dict-call curr render-phase-bstep 'bstep)
            ,@(dict-call curr render-phase-branches 'branch)
            ,@(dict-call curr render-phase-sampling 'sampling)
            ,@(dict-call curr (curryr simple-render-phase "Symmetry") 'symmetry)
            ,@(dict-call curr (curryr simple-render-phase "Remove") 'remove-preprocessing)
            ,@(dict-call curr render-phase-outcomes 'outcomes)
            ,@(dict-call curr render-phase-compiler 'compiler)
            ,@(dict-call curr render-phase-mixed-sampling 'mixsample)
            ,@(dict-call curr render-phase-bogosity 'bogosity))))

(define (if-cons test x l)
  (if test
      (cons x l)
      l))

(define (dict-call d f #:default [default '()] #:extra [extra (void)] . args)
  (if (andmap (curry dict-has-key? d) args)
      (apply f (if-cons (not (void? extra)) extra (map (curry dict-ref d) args)))
      default))

(define (render-phase-algorithm algorithm)
  `((dt "Algorithm")
    (dd (table ((class "times"))
               ,@(for/list ([alg (in-list (sort (group-by identity algorithm) > #:key length))])
                   `(tr (td ,(~r (length alg) #:group-sep " ") "×") (td ,(~a (car alg)))))))))

(define (render-phase-bogosity bogosity)
  (match-define (list domain-info) bogosity)
  (define total (round (apply + (hash-values domain-info))))

  (define tags '(valid unknown infinite unsamplable invalid precondition))

  `((dt "Bogosity") (dd (div ((class "bogosity"))
                             ,@(for/list ([tag tags])
                                 `(div ((class ,(format "bogosity-~a" tag))
                                        [data-id ,(format "bogosity-~a" tag)]
                                        [data-type ,(~a tag)]
                                        [data-timespan ,(~a (hash-ref domain-info tag 0))]
                                        [title
                                         ,(format "~a (~a)"
                                                  tag
                                                  (format-percent (hash-ref domain-info tag 0)
                                                                  total))])))))))

(define (render-phase-locations locations)
  `((dt "Localize:") (dd (p "Found " ,(~a (length locations)) " expressions of interest:")
                         (table ((class "times"))
                                (thead (tr (th "New") (th "Metric") (th "Score") (th "Program")))
                                ,@
                                (for/list ([rec (in-list locations)])
                                  (match-define (list expr metric score) rec)
                                  `(tr (td ,(~a metric)) (td ,(~a score)) (td (pre ,(~a expr)))))))))

(define (format-value v)
  (cond
    [(real? v) (~a v)]
    [(equal? (hash-ref v 'type) "real") (hash-ref v 'value)]
    [else
     (define repr-name (hash-ref v 'type))
     (define repr (get-representation (read (open-input-string repr-name))))
     (value->string ((representation-ordinal->repr repr) (string->number (hash-ref v 'ordinal)))
                    repr)]))

(define (render-phase-bstep iters)
  `((dt "Steps") (dd (table (tr (th "Time") (th "Left") (th "Right"))
                            ,@(for/list ([rec (in-list iters)])
                                (match-define (list time v1 v2) rec)
                                `(tr (td ,(format-time time))
                                     (td (pre ,(format-value v1)))
                                     (td (pre ,(format-value v2)))))))))

(define (render-phase-egraph iters)
  (define costs (map third iters))
  (define last-useful-iter (last (filter (compose (curry = (apply min costs)) third) iters)))
  `((dt "Iterations") (dd (p "Useful iterations: "
                             ,(~a (first last-useful-iter))
                             " ("
                             ,(format-time (fourth last-useful-iter))
                             ")")
                          (table ((class "times"))
                                 (tr (th "Iter") (th "Nodes") (th "Cost"))
                                 ,@(for/list ([rec (in-list (reverse iters))])
                                     (match-define (list iter nodes cost t) rec)
                                     `(tr (td ,(~a iter)) (td ,(~a nodes)) (td ,(~a cost))))))))

(define (render-phase-stop data)
  (match-define (list (list reasons counts) ...) (sort data > #:key second))
  `((dt "Stop Event") (dd (table ((class "times"))
                                 ,@(for/list ([reason reasons]
                                              [count counts])
                                     `(tr (td ,(~r count #:group-sep " ") "×") (td ,(~a reason))))))))

(define (format-percent num den)
  (string-append (if (zero? den)
                     (cond
                       [(positive? num) "+∞"]
                       [(zero? num) "0"]
                       [(negative? num) "-∞"])
                     (~r (* (/ num den) 100) #:precision 1))
                 "%"))

(define (average . values)
  (/ (apply + values) (length values)))

(define (render-phase-mixed-sampling mixsample)
  (define total-time (apply + (map first mixsample)))
  `((dt "Precisions")
    (dd (details
         (summary "Click to see histograms. Total time spent on operations: "
                  ,(format-time total-time))
         ,@(map first
                (sort (for/list ([rec (in-list (group-by second mixsample))]) ; group by operator
                        ; rec = '('(time op precision) ... '(time op precision))
                        (define n (random 100000))
                        (define op (second (car rec)))
                        (define precisions (map third rec))
                        (define times (map first rec))
                        (define time-per-op (round (apply + times)))

                        (list `(details (summary (code ,op)
                                                 ": "
                                                 ,(format-time time-per-op)
                                                 " ("
                                                 ,(format-percent time-per-op total-time)
                                                 " of total)")
                                        (canvas ([id ,(format "calls-~a" n)]
                                                 [title
                                                  "Histogram of precisions of the used operation"]))
                                        (script "histogram2D(\""
                                                ,(format "calls-~a" n)
                                                "\", "
                                                ,(jsexpr->string precisions)
                                                ", "
                                                ,(jsexpr->string times)
                                                ", "
                                                "{\"max\" : "
                                                ,(~a (*max-mpfr-prec*))
                                                "})"))
                              time-per-op))
                      >
                      #:key second))))))

(define (render-phase-sampling sampling)
  (define total (round (apply + (hash-values (cadr (car sampling))))))
  (define fields
    '(("Valid" . valid) ("Unknown" . unknown)
                        ("Precondition" . precondition)
                        ("Infinite" . infinite)
                        ("Domain" . invalid)
                        ("Can't" . unsamplable)))
  `((dt "Search") (dd (table ((class "times"))
                             (tr (th "Probability")
                                 ,@(for/list ([(name sym) (in-dict fields)])
                                     `(th ,name))
                                 (th "Iter"))
                             ,@(for/list ([(n table) (in-dict (sort sampling < #:key first))])
                                 `(tr (td ,(format-percent (hash-ref (car table) 'valid 0)
                                                           (+ (hash-ref (car table) 'valid 0)
                                                              (hash-ref (car table) 'unknown 0))))
                                      ,@(for/list ([(name sym) (in-dict fields)])
                                          `(td ,(format-percent (hash-ref (car table) sym 0) total)))
                                      (td ,(~a n))))))))

(define (simple-render-phase info name)
  (if (> (length (first info)) 0)
      `((dt ,name) (dd ,@(map (lambda (s) `(p ,(~a s))) (first info))))
      empty))

(define (render-phase-accuracy accuracy oracle baseline name link repr-name)
  (define rows
    (sort (for/list ([acc accuracy]
                     [ora oracle]
                     [bas baseline]
                     [name name]
                     [link link])
            (list (- acc ora) (- bas acc) link name))
          >
          #:key first))

  (define bits (map first rows))
  (define total-remaining (apply + accuracy))
  (define repr (get-representation (read (open-input-string (car repr-name)))))

  `((dt "Accuracy") (dd (p "Total "
                           ,(format-bits (apply + bits) #:unit #t)
                           " remaining"
                           " ("
                           ,(format-percent (apply + bits) total-remaining)
                           ")"
                           (p "Threshold costs "
                              ,(format-cost (apply + (filter (curry > 1) bits)) repr)
                              "b"
                              " ("
                              ,(format-percent (apply + (filter (curry > 1) bits)) total-remaining)
                              ")")
                           ,@(if (> (length rows) 1)
                                 `((table ((class "times"))
                                          ,@(for/list ([rec (in-list rows)]
                                                       [_ (in-range 5)])
                                              (match-define (list left gained link name) rec)
                                              `(tr (td ,(format-bits left #:unit #t))
                                                   (td ,(format-percent gained (+ left gained)))
                                                   (td (a ([href ,(format "~a/graph.html" link)])
                                                          ,(or name "")))))))
                                 '())))))

(define (render-phase-pruning kept-data)
  (match-define (list kept) kept-data)
  (define (altnum kind [col #f])
    (define rec (hash-ref kept kind))
    (match col
      [#f (first rec)]
      [0 (- (first rec) (second rec))]
      [1 (second rec)]))
  (define kept-alts (+ (altnum 'new 1) (altnum 'fresh 1)))
  (define done-alts (+ (altnum 'done 1) (altnum 'picked 1)))
  `((dt "Pruning")
    (dd (p ,(~a (+ kept-alts done-alts))
           " alts after pruning ("
           ,(~a kept-alts)
           " fresh and "
           ,(~a done-alts)
           " done)")
        (table
         ((class "states"))
         (thead (tr (th) (th "Pruned") (th "Kept") (th "Total")))
         (tbody ,@(for/list ([type '(new fresh picked done)])
                    `(tr (th ,(string-titlecase (~a type)))
                         (td ,(~r (altnum type 0) #:group-sep " "))
                         (td ,(~r (altnum type 1) #:group-sep " "))
                         (td ,(~r (altnum type) #:group-sep " ")))))
         (tfoot
          (tr (th "Total")
              (td ,(~r (apply + (map (curryr altnum 0) '(new fresh picked done))) #:group-sep " "))
              (td ,(~r (apply + (map (curryr altnum 1) '(new fresh picked done))) #:group-sep " "))
              (td ,(~r (apply + (map altnum '(new fresh picked done))) #:group-sep " "))))))))

(define (render-phase-memory mem)
  (match-define (list live alloc) (car mem))
  `((dt "Memory") (dd ,(~r (/ live (expt 2 20)) #:group-sep " " #:precision '(= 1))
                      "MiB live, "
                      ,(~r (/ alloc (expt 2 20)) #:group-sep " " #:precision '(= 1))
                      "MiB allocated")))

(define (render-phase-error min-error-table)
  (match-define (list min-error repr-name) (car min-error-table))
  (define repr (get-representation (read (open-input-string repr-name))))
  `((dt "Accuracy") (dd ,(format-accuracy min-error (representation-total-bits repr) #:unit "%") "")))

(define (render-phase-rules rules)
  `((dt "Rules") (dd (table ((class "times"))
                            ,@(for/list ([rec (in-list (sort rules > #:key second))]
                                         [_ (in-range 5)])
                                (match-define (list rule count) rec)
                                `(tr (td ,(~r count #:group-sep " ") "×")
                                     (td (code ,(~a rule) " "))))))))

(define (render-phase-fperrors fperrors)
  `((dt "FPErrors") (dd (details (summary "Click to see full error table")
                                 (table ((class "times"))
                                        (thead (tr (th "Ground Truth")
                                                   (th "Overpredictions")
                                                   (th "Example")
                                                   (th "Underpredictions")
                                                   (th "Example")
                                                   (th "Subexpression")))
                                        ,@(for/list ([rec (in-list (sort fperrors > #:key second))])
                                            (match-define (list expr tcount opred oex upred uex) rec)
                                            `(tr (td ,(~a tcount))
                                                 (td ,(~a opred))
                                                 (td ,(if oex
                                                          (~a oex)
                                                          "-"))
                                                 (td ,(~a upred))
                                                 (td ,(if uex
                                                          (~a uex)
                                                          "-"))
                                                 (td ,(if expr
                                                          `(code ,expr)
                                                          "No Errors")))))))))

(define (render-phase-explanations explanations)
  `((dt "Explanations")
    (dd (details
         (summary "Click to see full explanations table")
         (table ((class "times"))
                (thead (tr (th "Operator") (th "Subexpression") (th "Explanation") (th "Count")))
                ,@(append*
                   (for/list ([rec (in-list (sort explanations > #:key fourth))])
                     (match-define (list op expr expl cnt mcnt flows locations) rec)

                     (append (list `(tr (td (code ,(~a op)))
                                        (td (code ,(~a expr)))
                                        (td (b ,(~a expl)))
                                        (td ,(~a cnt))
                                        (td ,(~a mcnt))))
                             (for/list ([flow (in-list (or flows '()))])
                               (match-define (list ex type v) flow)
                               `(tr (td "↳") (td (code ,(~a ex))) (td ,type) (td ,(~a v))))))))))))

(define (render-phase-confusion confusion-matrix)
  (match-define (list (list true-pos false-neg false-pos true-neg)) confusion-matrix)
  `((dt "Confusion") (dd (table ((class "times"))
                                (tr (th "") (th "Predicted +") (th "Predicted -"))
                                (tr (th "+") (td ,(~a true-pos)) (td ,(~a false-neg)))
                                (tr (th "-") (td ,(~a false-pos)) (td ,(~a true-neg)))))
                     (dt "Precision")
                     (dd ,(if (= true-pos false-pos 0)
                              "0/0"
                              (~a (exact->inexact (/ true-pos (+ true-pos false-pos))))))
                     (dt "Recall")
                     (dd ,(if (= true-pos false-neg 0)
                              "0/0"
                              (~a (exact->inexact (/ true-pos (+ true-pos false-neg))))))))

(define (render-phase-maybe-confusion confusion-matrix)
  (match-define (list (list true-pos true-maybe false-neg false-pos false-maybe true-neg))
    confusion-matrix)
  `((dt "Confusion?")
    (dd (table ((class "times"))
               (tr (th "") (th "Predicted +") (th "Predicted Maybe") (th "Predicted -"))
               (tr (th "+") (td ,(~a true-pos)) (td ,(~a true-maybe)) (td ,(~a false-neg)))
               (tr (th "-") (td ,(~a false-pos)) (td ,(~a false-maybe)) (td ,(~a true-neg)))))
    (dt "Precision?")
    (dd ,(if (= true-pos true-maybe false-pos false-maybe 0)
             "0/0"
             (~a (exact->inexact (/ (+ true-pos true-maybe)
                                    (+ true-pos true-maybe false-pos false-maybe))))))
    (dt "Recall?")
    (dd ,(if (= true-pos true-maybe false-neg 0)
             "0/0"
             (~a (exact->inexact (/ (+ true-pos true-maybe) (+ true-pos true-maybe false-neg))))))))

(define (render-phase-total-confusion confusion-matrix)
  (match-define (list (list true-pos true-maybe false-neg false-pos false-maybe true-neg))
    confusion-matrix)
  `((dt "Total Confusion?")
    (dd (table ((class "times"))
               (tr (th "") (th "Predicted +") (th "Predicted Maybe") (th "Predicted -"))
               (tr (th "+") (td ,(~a true-pos)) (td ,(~a true-maybe)) (td ,(~a false-neg)))
               (tr (th "-") (td ,(~a false-pos)) (td ,(~a false-maybe)) (td ,(~a true-neg)))))
    (dt "Precision?")
    (dd ,(if (= true-pos true-maybe false-pos false-maybe 0)
             "0/0"
             (~a (exact->inexact (/ (+ true-pos true-maybe)
                                    (+ true-pos true-maybe false-pos false-maybe))))))
    (dt "Recall?")
    (dd ,(if (= true-pos true-maybe false-neg 0)
             "0/0"
             (~a (exact->inexact (/ (+ true-pos true-maybe) (+ true-pos true-maybe false-neg))))))))

(define (render-phase-freqs freqs)
  `((dt "Freqs") (dd "test"
                     (table ((class "times"))
                            (tr (th "number") (th "freq"))
                            ,@(for/list ([freq (in-list (sort freqs < #:key first))])
                                (match-define (list key val) freq)
                                `(tr (td ,(~a key) (td ,(~a val)))))))))

(define (render-phase-counts alts)
  (match-define (list (list inputs outputs)) alts)
  `((dt "Counts") (dd ,(~r inputs #:group-sep " ") " → " ,(~r outputs #:group-sep " "))))

(define (render-phase-alts alts)
  `((dt "Alt Table")
    (dd (details
         (summary "Click to see full alt table")
         (table ((class "times"))
                (thead (tr (th "Status") (th "Accuracy") (th "Program")))
                ,@(for/list ([rec (in-list alts)])
                    (match-define (list expr status score repr-name) rec)
                    (define repr (get-representation (read (open-input-string repr-name))))
                    `(tr ,(match status
                            ["next" `(td (span ([title "Selected for next iteration"]) "▶"))]
                            ["done" `(td (span ([title "Selected in a prior iteration"]) "✓"))]
                            ["fresh" `(td)])
                         (td ,(format-accuracy score (representation-total-bits repr) #:unit "%") "")
                         (td (pre ,expr)))))))))

(define (render-phase-times n times)
  `((dt "Calls")
    (dd (p ,(~r (length times) #:group-sep " ") " calls:")
        (canvas ([id ,(format "calls-~a" n)]
                 [title
                  "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
        (script "histogram(\"" ,(format "calls-~a" n) "\", " ,(jsexpr->string (map first times)) ")")
        (table ((class "times"))
               ,@(for/list ([rec (in-list (sort times > #:key first))]
                            [_ (in-range 5)])
                   (match-define (list time expr) rec)
                   `(tr (td ,(format-time time)) (td (pre ,(~a expr)))))))))

(define (render-phase-series n times)
  `((dt "Calls")
    (dd
     (p ,(~a (length times)) " calls:")
     (canvas ([id ,(format "calls-~a" n)]
              [title
               "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
     (script "histogram(\"" ,(format "calls-~a" n) "\", " ,(jsexpr->string (map first times)) ")")
     (table
      ((class "times"))
      (thead (tr (th "Time") (th "Variable") (th) (th "Point") (th "Expression")))
      ,@
      (for/list ([rec (in-list (sort times > #:key first))]
                 [_ (in-range 5)])
        (match-define (list time expr var transform) rec)
        `(tr (td ,(format-time time)) (td (pre ,var)) (td "@") (td ,transform) (td (pre ,expr))))))))

(define (render-phase-compiler compiler)
  (match-define (list (list sizes compileds) ...) compiler)
  (define size (apply + sizes))
  (define compiled (apply + compileds))
  `((dt "Compiler") (dd (p "Compiled "
                           ,(~r size #:group-sep " ")
                           " to "
                           ,(~r compiled #:group-sep " ")
                           " computations "
                           "("
                           ,(format-percent (- size compiled) size)
                           " saved)"))))

(define (render-phase-branches branches)
  `((dt "Results")
    (dd (table ((class "times"))
               (thead (tr (th "Accuracy") (th "Segments") (th "Branch")))
               ,@(for/list ([rec (in-list branches)])
                   (match-define (list expr score splits repr-name) rec)
                   (define repr (get-representation (read (open-input-string repr-name))))
                   `(tr (td ,(format-accuracy score (representation-total-bits repr) #:unit "%") "")
                        (td ,(~a splits))
                        (td (code ,expr))))))))

(define (render-phase-outcomes outcomes)
  `((dt "Samples") (dd (table ((class "times"))
                              ,@(for/list ([rec (in-list (sort outcomes > #:key first))])
                                  (match-define (list time precision category count) rec)
                                  `(tr (td ,(format-time time))
                                       (td ,(~r count #:group-sep " ") "×")
                                       (td ,(~a precision))
                                       (td ,(~a category))))))))

(define (render-phase-inputs inputs outputs)
  `((dt "Calls") (dd ,@(for/list ([call inputs]
                                  [output outputs]
                                  [n (in-naturals 1)])
                         `(details (summary "Call " ,(~a n))
                                   (table (thead (tr (th "Inputs")))
                                          ,@(for/list ([arg call])
                                              `(tr (td (pre ,(~a arg))))))
                                   (table (thead (tr (th "Outputs")))
                                          ,@(for/list ([out output])
                                              `(tr (td (pre ,(~a out)))))))))))

;; This next part handles summarizing several timelines into one details section for the report page.

(define (render-about info)
  (match-define (report-info date
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
    info)

  `(table ((id "about"))
          (tr (th "Date:") (td ,(date->string date)))
          (tr (th "Commit:")
              (td (abbr ([title ,commit])
                        ,(with-handlers ([exn:fail:contract? (const commit)])
                           (substring commit 0 8)))
                  " on "
                  ,branch))
          (tr (th "Hostname:") (td ,hostname " with Racket " ,(version)))
          (tr (th "Seed:") (td ,(~a seed)))
          (tr (th "Parameters:")
              (td ,(~a (*num-points*)) " points for " ,(~a (*num-iterations*)) " iterations"))
          (tr (th "Flags:")
              (td ((id "flag-list"))
                  (div ((id "all-flags"))
                       ,@(for*/list ([(class flags) (*flags*)]
                                     [flag flags])
                           `(kbd ,(~a class) ":" ,(~a flag))))
                  (div ((id "changed-flags"))
                       ,@(if (null? (changed-flags))
                             '("default")
                             (for/list ([rec (in-list (changed-flags))])
                               (match-define (list delta class flag) rec)
                               `(kbd ,(match delta
                                        ['enabled "+o"]
                                        ['disabled "-o"])
                                     " "
                                     ,(~a class)
                                     ":"
                                     ,(~a flag)))))))))

(define (render-profile)
  `(section ([id "profile"]) (h1 "Profiling") (p ((class "load-text")) "Loading profile data...")))
