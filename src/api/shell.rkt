#lang racket

(require "../syntax/read.rkt"
         "../syntax/types.rkt"
         "../syntax/sugar.rkt"
         "../utils/common.rkt"
         "datafile.rkt"
         "sandbox.rkt"
         "server.rkt")
(provide run-shell
         run-improve)

(define (unparse-result row)
  (define vars (table-row-vars row))
  (define repr (get-representation (table-row-precision row)))
  (define ctx (context vars repr (map (const repr) vars))) ; TODO: this seems wrong
  (define expr (or (table-row-output row) (table-row-input row)))
  `(FPCore ,@(filter identity (list (table-row-identifier row)))
           ,vars
           :herbie-status
           ,(string->symbol (table-row-status row))
           :herbie-time
           ,(table-row-time row)
           :herbie-error-input
           ([,(*num-points*) ,(table-row-start-est row)] [,(*reeval-pts*) ,(table-row-start row)])
           :herbie-error-output
           ([,(*num-points*) ,(table-row-result-est row)] [,(*reeval-pts*) ,(table-row-result row)])
           ,@(append (for/list ([rec (in-list (table-row-target row))])
                       (match-define (list cost score) rec)
                       `(:herbie-error-target ([,(*reeval-pts*) ,(table-row-target row)]))))
           ,@(if (empty? (table-row-warnings row))
                 '()
                 `(:herbie-warnings ,(table-row-warnings row)))
           :name
           ,(table-row-name row)
           :precision
           ,(table-row-precision row)
           ,@(if (eq? (table-row-pre row) 'TRUE)
                 '()
                 `(:pre ,(table-row-pre row)))
           ,@(if (equal? (table-row-preprocess row) empty)
                 '()
                 `(:herbie-preprocess ,(table-row-preprocess row)))
           ,@(append (for/list ([(target enabled?) (in-dict (table-row-target-prog row))]
                                #:when enabled?)
                       `(:alt ,target)))
           ,(prog->fpcore expr ctx)))

(define (get-shell-input)
  (printf "herbie> ")
  (with-handlers ([(or/c exn:fail:user? exn:fail:read?) (λ (e)
                                                          ((error-display-handler) (exn-message e) e)
                                                          (get-shell-input))])
    (define input
      (parameterize ([read-decimal-as-inexact false])
        (read-syntax "stdin" (current-input-port))))
    (cond
      [(eof-object? input)
       (printf "\n")
       eof]
      [else (parse-test input)])))

(define (print-improve-outputs tests results p #:seed [seed #f])
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
      (start-job 'improve test #:seed seed #:pcontext #f #:profile? #f #:timeline-disabled? #f)))
  (define results
    (for/list ([id ids])
      (get-table-data-from-hash (wait-for-job id) "")))

  (if (equal? output "-")
      (print-improve-outputs tests results (current-output-port) #:seed seed)
      (call-with-output-file output
                             #:exists 'replace
                             (λ (p) (print-improve-outputs tests results p #:seed seed)))))

(define (run-shell)
  (define seed (get-seed))
  (eprintf "Herbie ~a with seed ~a\n" *herbie-version* seed)
  (eprintf "Find help on https://herbie.uwplse.org/, exit with ~a\n"
           (match (system-type 'os)
             ['windows "Ctrl-Z Enter"]
             [_ "Ctrl-D"]))
  (start-job-server #f)
  (with-handlers ([exn:break? (λ (e) (exit 0))])
    (for ([test (in-producer get-shell-input eof-object?)]
          [idx (in-naturals)])
      (define result (wait-for-job (start-job 'improve test #:seed seed)))
      (define status (hash-ref result 'status))
      (define time (hash-ref result 'time))
      (define table-data (get-table-data-from-hash result ""))
      (match status
        ['success (pretty-print (unparse-result table-data) (current-output-port) 1)]
        ['failure
         (match-define (list 'exn type msg url locs traceback) (hash-ref result 'backend))
         (printf "; ~a\n" msg)
         (for ([loc (in-list locs)])
           (match-define (list msg file line col pos) loc)
           (printf ";   ~a:~a~a: ~a\n" file line col msg))
         (printf "; See <https://herbie.uwplse.org/doc/~a/~a> for more.\n" *herbie-version* url)]
        ['timeout (printf "Timeout in ~as (see --timeout option)\n" (/ time 1000))]
        [else (error 'run-shell "unknown result type ~a" status)]))))
