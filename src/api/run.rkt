#lang racket

(require json)
(require "../utils/common.rkt"
         "../syntax/read.rkt"
         "../syntax/sugar.rkt"
         "datafile.rkt"
         "../syntax/types.rkt"
         "../utils/profile.rkt"
         "../utils/timeline.rkt"
         "../core/sampling.rkt"
         "../reports/pages.rkt"
         "thread-pool.rkt"
         "../reports/timeline.rkt"
         "../reports/common.rkt")

(provide make-report
         rerun-report
         diff-report)

(define (extract-test row)
  (define vars (table-row-vars row))
  (define repr (get-representation (table-row-precision row)))
  (define var-reprs (map (curryr cons repr) vars))
  (define ctx (context vars repr (map (const repr) vars)))
  (test (table-row-name row)
        (table-row-identifier row)
        (table-row-vars row)
        (fpcore->prog (table-row-input row) ctx)
        (fpcore->prog (table-row-output row) ctx)
        (table-row-target-prog row)
        (fpcore->prog (table-row-spec row) ctx)
        (fpcore->prog (table-row-pre row) ctx)
        (table-row-preprocess row)
        (representation-name repr)
        (for/list ([(k v) (in-dict var-reprs)])
          (cons k (representation-name v)))
        (table-row-conversions row)))

(define (make-report bench-dirs #:dir dir #:note note #:threads threads)
  (define tests (reverse (sort (append-map load-tests bench-dirs) test<?)))
  (run-tests tests #:dir dir #:note note #:threads threads))

(define (rerun-report json-file #:dir dir #:note note #:threads threads)
  (define data (read-datafile json-file))
  (define tests (map extract-test (report-info-tests data)))
  (*flags* (report-info-flags data))
  (set-seed! (report-info-seed data))
  (*num-points* (report-info-points data))
  (*num-iterations* (report-info-iterations data))
  (run-tests tests #:dir dir #:note note #:threads threads))

(define (read-json-files info dir name)
  (filter identity
          (for/list ([res (report-info-tests info)])
            (define out
              (with-handlers ([exn? (const #f)])
                (call-with-input-file (build-path dir (table-row-link res) name) read-json)))
            (and out (not (eof-object? out)) (cons (table-row-link res) out)))))

(define (merge-timeline-jsons tl)
  (apply timeline-merge (map timeline-relink (dict-keys tl) (dict-values tl))))

(define (merge-profile-jsons ps)
  (profile->json (apply profile-merge (map json->profile (dict-values ps)))))

(define (run-tests tests #:dir dir #:note note #:threads threads)
  (define seed (get-seed))
  (when (not (directory-exists? dir))
    (make-directory dir))

  (define results (get-test-results tests #:threads threads #:seed seed #:profile true #:dir dir))
  (define info (make-report-info (filter values results) #:note note #:seed seed))

  (write-datafile (build-path dir "results.json") info)
  (copy-file (web-resource "report-page.js") (build-path dir "report-page.js") #t)
  (copy-file (web-resource "report.js") (build-path dir "report.js") #t)
  (copy-file (web-resource "report.css") (build-path dir "report.css") #t)
  (copy-file (web-resource "logo-car.png") (build-path dir "logo-car.png") #t)
  (copy-file (web-resource "report.html") (build-path dir "index.html") #t)
  (define timeline (merge-timeline-jsons (read-json-files info dir "timeline.json")))
  (call-with-output-file (build-path dir "timeline.json")
                         (curry write-json timeline)
                         #:exists 'replace)
  (define profile (merge-profile-jsons (read-json-files info dir "profile.json")))
  (call-with-output-file (build-path dir "profile.json") (curry write-json profile) #:exists 'replace)

  (call-with-output-file
   (build-path dir "timeline.html")
   #:exists 'replace
   (λ (out) (write-html (make-timeline "Herbie run" timeline #:info info #:path ".") out)))

  ; Delete old files
  (let* ([expected-dirs (map string->path
                             (filter identity (map table-row-link (report-info-tests info))))]
         [actual-dirs (filter (λ (name) (directory-exists? (build-path dir name)))
                              (directory-list dir))]
         [extra-dirs (filter (λ (name) (not (member name expected-dirs))) actual-dirs)])
    (for ([subdir extra-dirs])
      (with-handlers ([exn:fail:filesystem? (const true)])
        (delete-directory/files (build-path dir subdir))))))

(define (test<? t1 t2)
  (cond
    [(and (test-output t1) (test-output t2)) (string<? (test-name t1) (test-name t2))]
    [(and (not (test-output t1)) (not (test-output t2))) (string<? (test-name t1) (test-name t2))]
    ; Put things with an output first
    [else (test-output t1)]))

(define (diff-report old new)
  (define df
    (diff-datafiles (read-datafile (build-path old "results.json"))
                    (read-datafile (build-path new "results.json"))))
  (copy-file (web-resource "report.html") (build-path new "index.html") #t))
