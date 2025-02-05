#lang racket

(require (only-in xml write-xexpr xexpr?))
(require "../utils/common.rkt"
         "../syntax/read.rkt"
         "common.rkt")

(provide make-traceback)

(define (make-traceback result-hash)
  (match (hash-ref result-hash 'status)
    ['timeout (render-timeout result-hash)]
    ['failure (render-failure result-hash)]
    [status (error 'make-traceback "unexpected status ~a" status)]))

(define (render-failure result-hash)
  (define test (hash-ref result-hash 'test))
  (define warnings (hash-ref result-hash 'warnings))
  (define backend (hash-ref result-hash 'backend))

  ; unpack the exception
  (match-define (list 'exn type msg url extra traceback) backend)

  `(html
    (head (meta ((charset "utf-8")))
          (title "Exception for " ,(~a (test-name test)))
          (link ((rel "stylesheet") (type "text/css") (href "../report.css")))
          ,@js-tex-include
          (script ([src "../report.js"])))
    (body ,(render-menu (~a (test-name test))
                        (list '("Report" . "../index.html") '("Metrics" . "timeline.html")))
          ,(render-warnings warnings)
          ,(render-specification test)
          ,(if type
               `(section ([id "user-error"] (class "error"))
                         (h2 ,(~a msg) " " (a ([href ,url]) "(more)"))
                         ,(if (eq? type 'syntax)
                              (render-syntax-errors msg extra)
                              ""))
               "")
          ,(if type
               ""
               `(,@(render-reproduction test #:bug? #t)
                 (section ([id "backtrace"]) (h2 "Backtrace") ,(render-traceback msg traceback)))))))

(define (render-syntax-errors msg locations)
  `(table (thead (th ([colspan "2"]) ,msg) (th "L") (th "C"))
          (tbody ,@(for/list ([location (in-list locations)])
                     (match-define (list msg src line col pos) location)
                     `(tr (td ((class "procedure")) ,(~a msg))
                          (td ,(~a src))
                          (td ,(or (~a line "")))
                          (td ,(or (~a col) (~a pos))))))))

(define (render-traceback msg traceback)
  `(table
    (thead (th ([colspan "2"]) ,msg) (th "L") (th "C"))
    (tbody
     ,@
     (for/list ([(name loc) (in-dict traceback)])
       (match loc
         [(list file line col)
          `(tr (td ((class "procedure")) ,(~a name)) (td ,(~a file)) (td ,(~a line)) (td ,(~a col)))]
         [#f `(tr (td ((class "procedure")) ,(~a name)) (td ([colspan "3"]) "unknown"))])))))

(define (render-timeout result-hash)
  (define test (hash-ref result-hash 'test))
  (define time (hash-ref result-hash 'time))
  (define warnings (hash-ref result-hash 'warnings))

  `(html (head (meta ((charset "utf-8")))
               (title "Exception for " ,(~a (test-name test)))
               (link ((rel "stylesheet") (type "text/css") (href "../report.css")))
               ,@js-tex-include
               (script ([src "../report.js"])))
         (body ,(render-menu (~a (test-name test))
                             (list '("Report" . "../index.html") '("Metrics" . "timeline.html")))
               ,(render-warnings warnings)
               ,(render-specification test)
               (section ([id "user-error"] (class "error"))
                        (h2 "Timeout after " ,(format-time time))
                        (p "Use the " (code "--timeout") " flag to change the timeout.")))))
