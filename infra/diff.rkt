#lang racket

(require "../src/api/datafile.rkt")

(define (field-equal? access t1 t2)
  (equal? (access t1) (access t2)))

;; Only interested in output
;; Only compare against runs with the same configuration
(define (datafile-tests-equal? df1 df2)
  (define tests1 (report-info-tests df1))
  (define tests2 (report-info-tests df2))
  (and (= (length tests1) (length tests2))
       (andmap (λ (t1 t2)
                 (and (field-equal? table-row-name t1 t2)
                      (field-equal? table-row-identifier t1 t2)
                      (field-equal? table-row-output t1 t2)
                      (field-equal? table-row-cost-accuracy t1 t2)))
               tests1
               tests2)))

(module+ main
  (command-line #:args (outdir1 outdir2)
                (define df1 (read-datafile (build-path outdir1 "results.json")))
                (define df2 (read-datafile (build-path outdir2 "results.json")))
                (cond
                  [(datafile-tests-equal? df1 df2) (printf "Matching output expressions\n")]
                  [else
                   (printf "Output expressions do not match!!\n")
                   (printf " datafile1: ~a\n" df1)
                   (printf " datafile1: ~a\n" df2)
                   (exit 1)])))
