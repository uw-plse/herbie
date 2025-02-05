#lang racket
(require json)
(require "../src/utils/common.rkt"
         "../src/utils/timeline.rkt"
         "../src/utils/profile.rkt"
         "../src/api/datafile.rkt"
         "../src/reports/timeline.rkt"
         "../src/syntax/load-plugin.rkt"
         "../src/reports/common.rkt")

(define (merge-timelines outdir . dirs)
  (define tls
    (filter (conjoin (negate eof-object?) identity)
            (for/list ([dir (in-list dirs)])
              (with-handlers ([exn? (const #f)])
                (define tl (call-with-input-file (build-path outdir dir "timeline.json") read-json))
                (timeline-relink dir tl)))))
  (define info (read-datafile (build-path outdir (first dirs) "results.json")))
  (define joint-tl (apply timeline-merge tls))
  (call-with-output-file (build-path outdir "timeline.json")
                         #:exists 'replace
                         (curry write-json joint-tl))
  (call-with-output-file (build-path outdir "timeline.html")
                         #:exists 'replace
                         (λ (out)
                           (write-html (make-timeline "Herbie run" joint-tl #:info info) out))))

(define (merge-profiles outdir . dirs)
  (define pfs
    (filter (conjoin (negate eof-object?) identity)
            (for/list ([dir (in-list dirs)])
              (with-handlers ([exn? (const #f)])
                (call-with-input-file (build-path outdir dir "profile.json") read-json)))))
  (define joint-pf (apply profile-merge (map json->profile pfs)))
  (call-with-output-file (build-path outdir "profile.json")
                         #:exists 'replace
                         (curry write-json (profile->json joint-pf))))

(define (merge-reports outdir name . dirs)
  (load-herbie-builtins)
  (define rss
    (filter (conjoin (negate eof-object?) identity)
            (for/list ([dir (in-list dirs)])
              (with-handlers ([exn? (const #f)])
                (let ([df (read-datafile (build-path outdir dir "results.json"))])
                  (if (eof-object? df)
                      eof
                      (cons df dir)))))))
  (define dfs (map car rss))
  (define joint-rs (merge-datafiles dfs #:dirs dirs #:name name))
  (write-datafile (build-path outdir "results.json") joint-rs)
  (copy-file (web-resource "report.html") (build-path outdir "index.html") #t))

(module+ main
  (define name #f)
  (command-line #:once-each [("--name") _name "Name for the merged report" (set! name _name)]
                #:args (outdir . dirs)
                (apply merge-reports outdir name dirs)
                (apply merge-timelines outdir dirs)
                (apply merge-profiles outdir dirs)
                (copy-file (web-resource "report.js") (build-path outdir "report.js") #t)
                (copy-file (web-resource "report-page.js") (build-path outdir "report-page.js") #t)
                (copy-file (web-resource "report.css") (build-path outdir "report.css") #t)
                (copy-file (web-resource "logo-car.png") (build-path outdir "logo-car.png") #t)))
