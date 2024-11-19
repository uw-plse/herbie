#lang racket

(require (only-in xml write-xexpr xexpr?)
         (only-in fpbench core->tex *expr-cse-able?* [core-common-subexpr-elim core-cse]))

(require "../utils/common.rkt"
         "../core/points.rkt"
         "../utils/float.rkt"
         "../utils/alternative.rkt"
         "../syntax/types.rkt"
         "../syntax/read.rkt"
         "../core/bsearch.rkt"
         "../api/sandbox.rkt"
         "common.rkt")

(provide make-graph
         dummy-graph)

(define (dummy-graph command)
  `(html (head (meta ([charset "utf-8"]))
               (title "Result page for the " ,(~a command) " command is not available right now.")
               ,@js-tex-include
               (script ([src "https://unpkg.com/mathjs@4.4.2/dist/math.min.js"]))
               (script ([src "https://unpkg.com/d3@6.7.0/dist/d3.min.js"]))
               (script ([src "https://unpkg.com/@observablehq/plot@0.4.3/dist/plot.umd.min.js"]))
               (link ([rel "stylesheet"] [type "text/css"] [href "../report.css"]))
               (script ([src "../report.js"])))
         (body (h2 "Result page for the " ,(~a command) " command is not available right now."))))

(define (make-graph result-hash output? profile?)
  (define backend (hash-ref result-hash 'backend))
  (define test (hash-ref result-hash 'test))
  (define time (hash-ref result-hash 'time))
  (define warnings (hash-ref result-hash 'warnings))
  (define repr (test-output-repr test))
  (define repr-bits (representation-total-bits repr))
  (define ctx (test-context test))
  (define identifier (test-identifier test))

  (define preprocessing (hash-ref backend 'preprocessing))
  (match-define (alt-analysis start-alt _ start-error) (hash-ref backend 'start))
  (define targets (hash-ref backend 'target))
  (define end (hash-ref backend 'end))
  (define bogosity (hash-ref backend 'bogosity))

  (define start-cost (alt-cost start-alt repr))

  (define list-target-error
    (for/list ([target targets])
      (alt-analysis-test-errors target)))

  (define list-target-cost
    (for/list ([target targets])
      (alt-cost (alt-analysis-alt target) repr)))

  (define end-exprs (hash-ref end 'end-exprs))
  (define end-errors (hash-ref end 'end-errors))
  (define end-costs (hash-ref end 'end-costs))

  (define speedup
    (let ([better (for/list ([err end-errors]
                             [cost end-costs]
                             #:when (<= (errors-score err) (errors-score start-error)))
                    (/ start-cost cost))])
      (and (not (null? better)) (apply max better))))

  (define end-error (car end-errors))

  `(html
    (head (meta ([charset "utf-8"]))
          (title "Result for " ,(~a (test-name test)))
          ,@js-tex-include
          (script ([src "https://unpkg.com/mathjs@4.4.2/dist/math.min.js"]))
          (script ([src "https://unpkg.com/d3@6.7.0/dist/d3.min.js"]))
          (script ([src "https://unpkg.com/@observablehq/plot@0.4.3/dist/plot.umd.min.js"]))
          (link ([rel "stylesheet"] [type "text/css"] [href "../report.css"]))
          (script ([src "../report.js"])))
    (body
     ,(render-menu #:path ".."
                   (~a (test-name test))
                   (if output?
                       (list '("Report" . "../index.html") '("Metrics" . "timeline.html"))
                       (list '("Metrics" . "timeline.html"))))
     (div ([id "large"])
          ,(render-comparison
            "Percentage Accurate"
            (format-accuracy (errors-score start-error) repr-bits #:unit "%")
            (format-accuracy (errors-score end-error) repr-bits #:unit "%")
            #:title
            (format "Minimum Accuracy: ~a → ~a"
                    (format-accuracy (apply max (map ulps->bits start-error)) repr-bits #:unit "%")
                    (format-accuracy (apply max (map ulps->bits end-error)) repr-bits #:unit "%")))
          ,(render-large "Time" (format-time time))
          ,(render-large "Alternatives" (~a (length end-exprs)))
          ,(if (*pareto-mode*)
               (render-large "Speedup"
                             (if speedup
                                 (~r speedup #:precision '(= 1))
                                 "N/A")
                             "×"
                             #:title "Relative speed of fastest alternative that improves accuracy.")
               ""))
     ,(render-warnings warnings)
     ,(render-specification test #:bogosity bogosity)
     (figure ([id "graphs"])
             (h2 "Local Percentage Accuracy vs "
                 (span ([id "variables"]))
                 (a ((class "help-button float") [href ,(doc-url "report.html#graph")]
                                                 [target "_blank"])
                    "?"))
             (svg)
             (div ([id "functions"]))
             (figcaption "The average percentage accuracy by input value. Horizontal axis shows "
                         "value of an input variable; the variable is choosen in the title. "
                         "Vertical axis is accuracy; higher is better. Red represent the original "
                         "program, while blue represents Herbie's suggestion. "
                         "These can be toggled with buttons below the plot. "
                         "The line is an average while dots represent individual samples."))
     (section ([id "cost-accuracy"] (class "section") [data-benchmark-name ,(~a (test-name test))])
              ; TODO : Show all Developer Target Accuracy
              (h2 "Accuracy vs Speed"
                  (a ((class "help-button float") [href ,(doc-url "report.html#cost-accuracy")]
                                                  [target "_blank"])
                     "?"))
              (div ((class "figure-row"))
                   (svg)
                   (div (p "Herbie found " ,(~a (length end-exprs)) " alternatives:")
                        (table (thead (tr (th "Alternative")
                                          (th ((class "numeric")) "Accuracy")
                                          (th ((class "numeric")) "Speedup")))
                               (tbody))))
              (figcaption "The accuracy (vertical axis) and speed (horizontal axis) of each "
                          "alternatives. Up and to the right is better. The red square shows "
                          "the initial program, and each blue circle shows an alternative."
                          "The line shows the best available speed-accuracy tradeoffs."))
     ,(let-values ([(dropdown body) (render-program (alt-expr start-alt) ctx #:ident identifier)])
        `(section ([id "initial"] (class "programs"))
                  (h2 "Initial Program"
                      ": "
                      (span ((class "subhead"))
                            (data ,(format-accuracy (errors-score start-error) repr-bits #:unit "%"))
                            " accurate, "
                            (data "1.0×")
                            " speedup")
                      ,dropdown
                      ,(render-help "report.html#alternatives"))
                  ,body))
     ,@(for/list ([i (in-naturals 1)]
                  [expr end-exprs]
                  [errs end-errors]
                  [cost end-costs]
                  [history (hash-ref end 'end-histories)])
         (define-values (dropdown body)
           (render-program expr ctx #:ident identifier #:instructions preprocessing))
         `(section ([id ,(format "alternative~a" i)] (class "programs"))
                   (h2 "Alternative "
                       ,(~a i)
                       ": "
                       (span ((class "subhead"))
                             (data ,(format-accuracy (errors-score errs) repr-bits #:unit "%"))
                             " accurate, "
                             (data ,(~r (/ (alt-cost start-alt repr) cost) #:precision '(= 1)) "×")
                             " speedup")
                       ,dropdown
                       ,(render-help "report.html#alternatives"))
                   ,body
                   (details (summary "Derivation") (ol ((class "history")) ,@history))))
     ,@(for/list ([i (in-naturals 1)]
                  [target (in-list targets)]
                  [target-error (in-list list-target-error)]
                  [target-cost (in-list list-target-cost)])
         (let-values ([(dropdown body)
                       (render-program (alt-expr (alt-analysis-alt target)) ctx #:ident identifier)])
           `(section
             ([id ,(format "target~a" i)] (class "programs"))
             (h2 "Developer Target "
                 ,(~a i)
                 ": "
                 (span ((class "subhead"))
                       (data ,(format-accuracy (errors-score target-error) repr-bits #:unit "%"))
                       " accurate, "
                       (data ,(~r (/ (alt-cost start-alt repr) target-cost) #:precision '(= 1)) "×")
                       " speedup")
                 ,dropdown
                 ,(render-help "report.html#target"))
             ,body)))
     ,(render-reproduction test))))
