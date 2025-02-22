#lang racket

(require json)
(require "../syntax/read.rkt"
         "timeline.rkt"
         "plot.rkt"
         "make-graph.rkt"
         "traceback.rkt"
         "common.rkt")

(provide all-pages
         make-page
         page-error-handler)

(define (all-pages result-hash)
  (define good? (eq? (hash-ref result-hash 'status) 'success))
  (define default-pages '("graph.html" "timeline.html" "timeline.json"))
  (define success-pages '("points.json" "profile.json"))
  (append default-pages (if good? success-pages empty)))

(define ((page-error-handler result-hash page out) e)
  (define test (hash-ref result-hash 'test))
  (eprintf "Error generating `~a` for \"~a\":\n  ~a\n" page (test-name test) (exn-message e))
  (eprintf "context:\n")
  (for ([(fn loc) (in-dict (continuation-mark-set->context (exn-continuation-marks e)))])
    (match loc
      [(srcloc file line col _ _) (eprintf "  ~a:~a:~a: ~a\n" file line col (or fn "(unnamed)"))]
      [#f (eprintf "  ~a\n" fn)]))
  (parameterize ([current-error-port out])
    (display "<!doctype html><pre>" out)
    ((error-display-handler) (exn-message e) e)
    (display "</pre>" out)))

(define (make-page page out result-hash output? profile?)
  (match page
    ["graph.html" (write-html (make-graph-html result-hash output? profile?) out)]
    ["timeline.html"
     (write-html (make-timeline (test-name (hash-ref result-hash 'test))
                                (hash-ref result-hash 'timeline)
                                #:path "..")
                 out)]
    ["timeline.json" (write-json (hash-ref result-hash 'timeline) out)]
    ["profile.json" (write-json (hash-ref result-hash 'profile) out)]
    ["points.json" (write-json (make-points-json result-hash) out)]))

(define (make-graph-html result-hash output? profile?)
  (define status (hash-ref result-hash 'status))
  (match status
    ['success
     (define command (hash-ref result-hash 'command))
     (match command
       ["improve" (make-graph result-hash output? profile?)]
       [else (dummy-graph command)])]
    ['timeout (make-traceback result-hash)]
    ['failure (make-traceback result-hash)]
    [_ (error 'make-graph-html "unknown result type ~a" status)]))
