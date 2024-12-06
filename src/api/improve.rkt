#lang racket
(require "../syntax/read.rkt"
         "../utils/common.rkt"
         "datafile.rkt"
         "sandbox.rkt"
         "server.rkt")
(provide run-improve)

(define (print-outputs tests results p #:seed [seed #f])
  (when seed
    (fprintf p ";; seed: ~a\n\n" seed))
  (for ([res results]
        [test tests]
        #:when res)
    (define name (table-row-name res))
    (match (table-row-status res)
      ["error"
       (fprintf p ";; Error in ~a\n" name)
       (write (unparse-result res) p)
       (newline p)]
      ["crash"
       (fprintf p ";; Crash in ~a\n" name)
       (write (unparse-result res) p)
       (newline p)]
      ["timeout"
       (fprintf p ";; ~a times out in ~as\n" (/ (*timeout*) 1000) name)
       (write (unparse-result res) p)
       (newline p)]
      [(? string?)
       (write (unparse-result res) p)
       (newline p)])))

(define (run-improve input output #:threads [threads #f])
  (define seed (get-seed))
  (define tests (load-tests input))
  (start-job-server threads)
  (define ids
    (for/list ([test (in-list tests)])
      (define command
        (create-job 'improve test #:seed seed #:pcontext #f #:profile? #f #:timeline-disabled? #f))
      (start-job command)))
  (define results
    (for/list ([id ids])
      (get-table-data-from-hash (wait-for-job id) "")))

  (if (equal? output "-")
      (print-outputs tests results (current-output-port) #:seed seed)
      (call-with-output-file output
                             #:exists 'replace
                             (λ (p) (print-outputs tests results p #:seed seed)))))
