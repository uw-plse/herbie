#lang racket

(require "../syntax/read.rkt"
         "sandbox.rkt"
         "../utils/common.rkt")
(provide run-shell)

(define (get-input)
  (printf "herbie> ")
  (with-handlers ([(or/c exn:fail:user? exn:fail:read?) (λ (e)
                                                          ((error-display-handler) (exn-message e) e)
                                                          (get-input))])
    (define input
      (parameterize ([read-decimal-as-inexact false])
        (read-syntax "stdin" (current-input-port))))
    (if (eof-object? input)
        (begin
          (printf "\n")
          eof)
        (parse-test input))))

(define (run-shell)
  (define seed (get-seed))
  (eprintf "Herbie ~a with seed ~a\n" *herbie-version* seed)
  (eprintf "Find help on https://herbie.uwplse.org/, exit with ~a\n"
           (match (system-type 'os)
             ['windows "Ctrl-Z Enter"]
             [_ "Ctrl-D"]))
  (with-handlers ([exn:break? (λ (e) (exit 0))])
    (for ([test (in-producer get-input eof-object?)]
          [idx (in-naturals)])
      (define result (run-herbie 'improve test #:seed seed))
      (define status (job-result-status result))
      (define time (job-result-time result))
      (match status
        ['success (pretty-print (unparse-result (get-table-data result "")) (current-output-port) 1)]
        ['failure
         (define exn (job-result-backend result))
         ((error-display-handler) (exn-message exn) exn)]
        ['timeout (printf "Timeout in ~as (see --timeout option)\n" (/ time 1000))]
        [else (error 'run-shell "unknown result type ~a" status)]))))
