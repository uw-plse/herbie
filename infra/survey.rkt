#lang racket
(require json
         xml)
(require "../src/api/datafile.rkt")

(define metrics
  (list `(start . ,table-row-start) `(end . ,table-row-result) `(time . ,table-row-time)))

(define (merge-results . dirs)
  (define results
    (filter (conjoin (negate eof-object?) identity)
            (for/list ([dir (in-list dirs)])
              (with-handlers ([exn? (const #f)])
                (read-datafile (build-path dir "results.json"))))))
  (for/hash ([group (group-by table-row-name (append-map report-info-tests results))])
    (values (table-row-name (first group))
            (for/hash ([(name metric) (in-dict metrics)])
              (values name (map metric group))))))

(define style
  "
section { overflow: auto; }
h2 { font-size: 100%; font-family: sans-serif; margin: 3em 0 0; }
h3 { text-transform: uppercase; font-size: 80%; color: steelblue}
section > div { width: 500; float: left; margin-right: 20px; }
")

(define (results->html results out)
  (fprintf out "<!doctype html>\n")
  (write-xexpr
   `(html
     (head (meta ([charset "utf-8"]))
           (title "Seed survey results")
           (style ,(make-cdata #f #f style))
           (script ([src "report.js"])))
     (body
      (h1 ,(format "Seed survey for ~a benchmarks" (hash-count results)))
      ,@
      (for/list ([(name metrics) (in-dict results)]
                 [n (in-naturals)])
        `(section
          (h2 ,(~a name))
          ,@
          (for/list ([(name values) (in-dict metrics)])
            `(div
              (h3 ,(~a name))
              (canvas
               ([id ,(format "~a-~a" name n)]
                [title
                 "Weighted histogram; height corresponds to percentage of runtime in that bucket."]))
              (script "histogram(\""
                      ,(format "~a-~a" name n)
                      "\", "
                      ,(jsexpr->string values)
                      ", "
                      ,(jsexpr->string (hash 'width
                                             400
                                             'proportional
                                             #f
                                             'buckets
                                             32.0
                                             'max
                                             (if (equal? name 'time)
                                                 (json-null)
                                                 64.0)))
                      ")")))))))
   out))

(module+ main
  (command-line #:args (outdir . dirs)
                (define data (apply merge-results dirs))
                (printf "Read in data on ~a items\n" (hash-count data))
                (call-with-output-file (build-path outdir "survey.html")
                                       #:exists 'replace
                                       (λ (p) (results->html data p)))))
