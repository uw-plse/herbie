#lang racket

(require json)
(require racket/exn)
(require openssl/sha1
         (rename-in xml [location? xml-location?]))
(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         web-server/dispatchers/dispatch
         web-server/dispatch/extend
         web-server/http/bindings
         web-server/configuration/responders
         web-server/managers/none
         web-server/safety-limits)

(require "../utils/common.rkt"
         "../config.rkt"
         "../syntax/read.rkt"
         "../utils/errors.rkt")
(require "../syntax/sugar.rkt"
         "../core/points.rkt")
(require "datafile.rkt"
         "../reports/pages.rkt"
         "../reports/common.rkt"
         "../reports/core2mathjs.rkt"
         "server.rkt")

(provide run-demo)

(define *demo-prefix* (make-parameter "/"))
(define *demo-log* (make-parameter false))

(define (add-prefix url)
  (string-replace (string-append (*demo-prefix*) url) "//" "/"))

(define-coercion-match-expander
 hash-arg/m
 (λ (x)
   (and (not (and (*demo-output*) ; If we've already saved to disk, skip this job
                  (directory-exists? (build-path (*demo-output*) x))))
        (let ([m (regexp-match #rx"^([0-9a-f]+)\\.[0-9a-f.]+" x)])
          (and m (server-check-on (second m))))))
 (λ (x)
   (let ([m (regexp-match #rx"^([0-9a-f]+)\\.[0-9a-f.]+" x)])
     (server-check-on (if m
                          (second m)
                          x)))))

(define-bidi-match-expander hash-arg hash-arg/m hash-arg/m)

(define-values (dispatch url*)
  (dispatch-rules [("") main]
                  [("check-status" (string-arg)) check-status]
                  [("timeline" (string-arg)) get-timeline]
                  [("up") check-up]
                  [((hash-arg) (string-arg)) generate-page]
                  [("results.json") generate-report]
                  [("improve") #:method (or "post" "get" "put") improve]
                  [("api" "result" (string-arg)) get-result]
                  [("api" "sample") #:method "post" sample-endpoint]
                  [("api" "explanations") #:method "post" explanations-endpoint]
                  [("api" "analyze") #:method "post" analyze-endpoint]
                  [("api" "exacts") #:method "post" exacts-endpoint]
                  [("api" "calculate") #:method "post" calculate-endpoint]
                  [("api" "localerror") #:method "post" local-error-endpoint]
                  [("api" "alternatives") #:method "post" alternatives-endpoint]
                  [("api" "cost") #:method "post" cost-endpoint]
                  [("api" "mathjs") #:method "post" mathjs-endpoint]
                  [("api" "translate") #:method "post" translate-endpoint]
                  [("api" "start" "improve") #:method "post" improve-start]
                  [("api" "start" "sample") #:method "post" start-sample-endpoint]
                  [("api" "start" "explanations") #:method "post" start-explanations-endpoint]
                  [("api" "start" "analyze") #:method "post" start-analyze-endpoint]
                  [("api" "start" "exacts") #:method "post" start-exacts-endpoint]
                  [("api" "start" "calculate") #:method "post" start-calculate-endpoint]
                  [("api" "start" "localerror") #:method "post" start-local-error-endpoint]
                  [("api" "start" "alternatives") #:method "post" start-alternatives-endpoint]
                  [("api" "start" "cost") #:method "post" start-cost-endpoint]))

(define (generate-page req job-id page)
  (define path (first (string-split (url->string (request-uri req)) "/")))
  (cond
    [(check-and-send path job-id page)]
    [else (next-dispatcher)]))

(define (check-and-send path job-id page)
  (define result-hash (get-results-for job-id))
  (cond
    [(set-member? (all-pages result-hash) page)
     ;; Write page contents to disk
     (when (*demo-output*)
       (write-results-to-disk result-hash path))
     (response 200
               #"OK"
               (current-seconds)
               #"text"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count)))))
               (λ (out)
                 (with-handlers ([exn:fail? (page-error-handler result-hash page out)])
                   (make-page page out result-hash (*demo-output*) #f))))]
    [else #f]))

(define (generate-report req)
  (cond
    [(and (*demo-output*) (file-exists? (build-path (*demo-output*) "results.json")))
     (next-dispatcher)]
    [else
     (define info
       (make-report-info (get-improve-table-data)
                         #:seed (get-seed)
                         #:note (if (*demo?*) "Web demo results" "Herbie results")))
     (response 200
               #"OK"
               (current-seconds)
               #"text"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count)))))
               (λ (out) (write-datafile out info)))]))

(define url (compose add-prefix url*))

(define (function-list . fn-classes)
  (define (fn->html fn)
    `(code ,(~a fn)))

  (define (fn-class class)
    (match-define (list fns description ...) class)
    `((dt () ,@(add-between (map fn->html fns) ", ")) (dd () ,@description)))

  `(dl ((class "function-list")) ,@(append-map fn-class fn-classes)))

(define (herbie-page #:title title
                     #:show-title [title? true]
                     #:scripts [scripts '()]
                     #:styles [styles '()]
                     #:head-include [other-include-head '()]
                     . body)
  `(html (head (meta ([charset "utf-8"]))
               (title ,title)
               ,@other-include-head
               ,@(for/list ([script scripts])
                   `(script ([src ,script] [type "text/javascript"])))
               (link ([rel "stylesheet"] [type "text/css"] [href "main.css"]))
               ,@(for/list ([style styles])
                   `(link ([rel "stylesheet"] [type "text/css"] [href ,style]))))
         (body (header (img ((class "logo") [src "/logo.png"]))
                       ,@(if title?
                             `((h1 ,title))
                             '()))
               ,@body)))

(define (main req)
  (when (and (*demo-output*) (not (directory-exists? (*demo-output*))))
    (make-directory (*demo-output*)))

  (response/xexpr
   #:headers (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count)))))
   (herbie-page
    #:title (if (*demo?*) "Herbie web demo" "Herbie")
    #:show-title (*demo?*)
    #:scripts '("//cdnjs.cloudflare.com/ajax/libs/mathjs/1.6.0/math.min.js" "demo.js")
    `(p
      "Write a formula below, and Herbie will try to improve it. Enter approximate ranges for inputs.")
    `(p ([id "options"])
        (a ([id "show-example"]) "Show an example")
        " | "
        (a ([id "use-fpcore"]) "Use FPCore"))
    (cond
      [(is-server-up)
       `(form
         ([action ,(url improve)] [method "post"] [id "formula"] [data-progress ,(url improve-start)])
         (textarea ([name "formula"] [autofocus "true"]
                                     [placeholder "(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))"]))
         (input ([name "formula-math"] [placeholder "sqrt(x + 1) - sqrt(x)"]))
         (table ([id "input-ranges"]))
         (ul ([id "errors"]))
         (ul ([id "warnings"]))
         (button ([id "run_herbie"] [type "submit"] [tabindex "-1"]) "Improve with Herbie")
         (pre ([id "progress"] [style "display: none;"])))]
      [(*demo?*)
       `(p ([id "crashed"])
           "Unfortunately, the online Herbie demo has crashed. The maintainers "
           "have been notified and will restart Herbie when they've got a moment, "
           "but if it's been a few days, it can help to "
           (a ([href "https://github.com/herbie-fp/herbie/issues/new"]
               [title "File a Herbie issue on Github"])
              "file an issue")
           ".")]
      [else
       `(p ([id "crashed"])
           "Unfortunately Herbie has crashed. You'll need to restart Herbie to "
           "continue using it. Please also "
           (a ([href "https://github.com/herbie-fp/herbie/issues/new"]
               [title "File a Herbie issue on Github"])
              "file a bug report")
           " with any error messages you find in your terminal.")])
    (if (*demo?*)
        `(p "To handle the high volume of requests, web requests are queued; "
            "there are "
            (span ([id "num-jobs"]) ,(~a (job-count)))
            " jobs in the queue right now. "
            "Web demo requests may also time out and cap the number of improvement iterations. "
            "To avoid these limitations, "
            (a ([href "/doc/latest/installing.html"]) "install Herbie")
            " on your own computer.")
        "")
    `(p ([id "lisp-instructions"])
        "Please enter formulas as "
        (a ([href "https://fpbench.org/spec/fpcore-1.0.html"]) "FPCore")
        " expressions, including the top-level "
        (code "FPCore")
        " form, "
        "using only the following supported functions:")
    `(p ([id "mathjs-instructions"] [style "display: none;"])
        "Use ordinary mathematical syntax (parsed by "
        (a ([href "https://mathjs.org"]) "math.js")
        ")"
        " and "
        (a ([href ,(format "https://herbie.uwplse.org/doc/~a/input.html" *herbie-version*)])
           "standard functions")
        " like:")
    (function-list '((+ - * / abs) "The usual arithmetic functions")
                   '((and or) "Logical connectives (for preconditions)")
                   '((pow) "Raising a value to a power")
                   '((exp log) "Natural exponent and natural log")
                   '((sin cos tan) "The trigonometric functions")
                   '((asin acos atan) "The inverse trigonometric functions")
                   '((sqrt cbrt) "Square and cube roots")
                   '((PI E) "The mathematical constants"))
    `(p (em "Note")
        ": "
        ,@(cond
            [(not (*demo-output*)) '("formulas submitted here are not logged.")]
            [(*demo?*)
             `("all formulas submitted here are logged and made public."
               (a ([href "./index.html"]) " See what formulas other users submitted."))]
            [else `("all formulas submitted here are " (a ([href "./index.html"]) "logged") ".")])))))

(define (post-with-json-response fn)
  (lambda (req)
    (define post-body (request-post-data/raw req))
    (define post-data
      (cond
        [post-body (bytes->jsexpr post-body)]
        [#t #f]))
    (define resp
      (with-handlers ([exn:fail? (λ (e) (hash 'error (exn->string e)))])
        (fn post-data)))
    (if (hash-has-key? resp 'error)
        (eprintf "Error handling request: ~a\n" (hash-ref resp 'error))
        (eprintf "Success handling request\n"))
    (if (hash-has-key? resp 'error)
        (response 500
                  #"Bad Request"
                  (current-seconds)
                  APPLICATION/JSON-MIME-TYPE
                  (list (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
                  (λ (op) (write-json resp op)))
        (response 200
                  #"OK"
                  (current-seconds)
                  APPLICATION/JSON-MIME-TYPE
                  (filter values
                          (list (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*"))
                                (and (hash-has-key? resp 'job)
                                     (header #"X-Herbie-Job-ID"
                                             (string->bytes/utf-8 (hash-ref resp 'job))))))
                  (λ (op) (write-json resp op))))))

(define (response/error title body)
  (response/full 400
                 #"Bad Request"
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 '()
                 (list (string->bytes/utf-8 (xexpr->string (herbie-page #:title title body))))))

(define (get-result req job-id)
  (match (get-results-for job-id)
    [#f
     (response 404
               #"Job Not Found"
               (current-seconds)
               #"text/plain"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count))))
                     (header #"X-Herbie-Job-ID" (string->bytes/utf-8 job-id))
                     (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
               (λ (out) `()))]
    [job-result
     (response 201
               #"Job complete"
               (current-seconds)
               #"text/plain"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count))))
                     (header #"X-Herbie-Job-ID" (string->bytes/utf-8 job-id))
                     (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
               (λ (out) (write-json job-result out)))]))

(define (improve-common req body go-back)
  (match (extract-bindings 'formula (request-bindings req))
    [(list formula-str)
     (define formula
       (with-handlers ([exn:fail? (λ (e) #f)])
         (read-syntax 'web (open-input-string formula-str))))
     (unless formula
       (raise-herbie-error "bad input: did you include special characters like `#`?"))
     (with-handlers ([exn:fail:user:herbie?
                      (λ (e)
                        (response/error
                         "Demo Error"
                         `(div
                           (h1 "Invalid formula")
                           (pre ,(herbie-error->string e))
                           (p "Formula must be a valid program using only the supported functions. "
                              "Please "
                              (a ([href ,go-back]) "go back")
                              " and try again."))))])
       (when (eof-object? formula)
         (raise-herbie-error "no formula specified"))
       (define test (parse-test formula))
       (define command
         (create-job 'improve
                     test
                     #:seed (get-seed)
                     #:pcontext #f
                     #:profile? #f
                     #:timeline-disabled? #f))
       (body (_create-job0 'herbie-command command)))]
    [_
     (response/error "Demo Error"
                     `(p "You didn't specify a formula (or you specified several). "
                         "Please "
                         (a ([href ,go-back]) "go back")
                         " and try again."))]))

(define (improve-start req)
  (improve-common
   req
   (λ (action)
     (define job-id (start-job action))
     (response/full 201
                    #"Job started"
                    (current-seconds)
                    #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (url check-status job-id)))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count))))
                          (header #"X-Herbie-Job-ID" (string->bytes/utf-8 job-id)))
                    '()))
   (url main)))

(define (check-status req job-id)
  (match (get-timeline-for job-id)
    [(? hash? result-hash)
     (response/full 201
                    #"Job complete"
                    (current-seconds)
                    #"text/plain"
                    (list (header #"Location"
                                  (string->bytes/utf-8
                                   (add-prefix (format "~a.~a/graph.html" job-id *herbie-commit*))))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count))))
                          (header #"X-Herbie-Job-ID" (string->bytes/utf-8 job-id))
                          (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
                    '())]
    [timeline
     (response 202
               #"Job in progress"
               (current-seconds)
               #"text/plain"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count))))
                     (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
               (λ (out)
                 (when timeline
                   (for ([entry timeline])
                     (fprintf out "Doing ~a\n" (hash-ref entry 'type))))))]))

(define (check-up req)
  (response/full (if (is-server-up) 200 500)
                 (if (is-server-up) #"Up" #"Down")
                 (current-seconds)
                 #"text/plain"
                 (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count))))
                       (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
                 '()))

(define (improve req)
  (improve-common req
                  (λ (action)
                    (define job-id (start-job action))
                    (wait-for-job job-id)
                    (redirect-to (add-prefix (format "~a.~a/graph.html" job-id *herbie-commit*))
                                 see-other))
                  (url main)))

(define (get-timeline req job-id)
  (match (get-results-for job-id)
    [#f
     (response 404
               #"Job Not Found"
               (current-seconds)
               #"text/plain"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count))))
                     (header #"X-Herbie-Job-ID" (string->bytes/utf-8 job-id))
                     (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
               (λ (out) `()))]
    [job-result
     (response 201
               #"Job complete"
               (current-seconds)
               #"text/plain"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (job-count))))
                     (header #"X-Herbie-Job-ID" (string->bytes/utf-8 job-id))
                     (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
               (λ (out) (write-json (hash-ref job-result 'timeline) out)))]))

; Macro for defining async and sync versions of an endpoint.
(define-syntax-rule (define-endpoint ([sync-name async-name] post-data) body ...)
  (begin
    (define (function post-data)
      body ...)
    (define sync-name
      (post-with-json-response (lambda (post-data)
                                 (define job-id (start-job (function post-data)))
                                 (wait-for-job job-id))))
    (define async-name
      (post-with-json-response (lambda (post-data)
                                 (define job-id (start-job (function post-data)))
                                 (hasheq 'job job-id 'path (make-path job-id)))))))

; /api/sample endpoint: test in console on demo page:
;; (await fetch('/api/sample', {method: 'POST', body: JSON.stringify({formula: "(FPCore (x) (- (sqrt (+ x 1))))", seed: 5})})).json()
(define-endpoint
 ([sample-endpoint start-sample-endpoint] post-data)
 (define test (get-test post-data))
 (define seed (parse-seed post-data))
 (_create-job0
  'herbie-command
  (create-job 'sample test #:seed seed #:pcontext #f #:profile? #f #:timeline-disabled? #t)))

; (create-job 'explanations (get-test post-data) #:seed (get-seed post-data) #:pcontext (get-pcontext post-data) #:profile? #f #:timeline-disabled? #t)
(define (get-test post-data)
  (define formula-str (hash-ref post-data 'formula))
  (define formula (read-syntax 'web (open-input-string formula-str)))
  (parse-test formula))

; The name get-seed is taken.
(define (parse-seed post-data)
  (hash-ref post-data 'seed #f))

(define (get-pcontext post-data)
  (define test (get-test post-data))
  (define sample (hash-ref post-data 'sample))
  (json->pcontext sample (test-context test)))

(define-endpoint ([explanations-endpoint start-explanations-endpoint] post-data)
                 (_create-job0 'herbie-command
                               (create-job 'explanations
                                           (get-test post-data)
                                           #:seed (parse-seed post-data)
                                           #:pcontext (get-pcontext post-data)
                                           #:profile? #f
                                           #:timeline-disabled? #t)))

(define-endpoint ([analyze-endpoint start-analyze-endpoint] post-data)
                 (_create-job0 'herbie-command
                               (create-job 'errors
                                           (get-test post-data)
                                           #:seed (parse-seed post-data)
                                           #:pcontext (get-pcontext post-data)
                                           #:profile? #f
                                           #:timeline-disabled? #t)))

;; (await fetch('/api/exacts', {method: 'POST', body: JSON.stringify({formula: "(FPCore (x) (- (sqrt (+ x 1))))", points: [[1, 1]]})})).json()
(define-endpoint ([exacts-endpoint start-exacts-endpoint] post-data)
                 (_create-job0 'herbie-command
                               (create-job 'exacts
                                           (get-test post-data)
                                           #:seed (parse-seed post-data)
                                           #:pcontext (get-pcontext post-data)
                                           #:profile? #f
                                           #:timeline-disabled? #t)))

(define-endpoint ([calculate-endpoint start-calculate-endpoint] post-data)
                 (_create-job0 'herbie-command
                               (create-job 'evaluate
                                           (get-test post-data)
                                           #:seed (parse-seed post-data)
                                           #:pcontext (get-pcontext post-data)
                                           #:profile? #f
                                           #:timeline-disabled? #t)))

(define-endpoint ([local-error-endpoint start-local-error-endpoint] post-data)
                 (_create-job0 'herbie-command
                               (create-job 'local-error
                                           (get-test post-data)
                                           #:seed (parse-seed post-data)
                                           #:pcontext (get-pcontext post-data)
                                           #:profile? #f
                                           #:timeline-disabled? #t)))

(define-endpoint ([alternatives-endpoint start-alternatives-endpoint] post-data)
                 (_create-job0 'herbie-command
                               (create-job 'alternatives
                                           (get-test post-data)
                                           #:seed (parse-seed post-data)
                                           #:pcontext (get-pcontext post-data)
                                           #:profile? #f
                                           #:timeline-disabled? #t)))

(define-endpoint ([cost-endpoint start-cost-endpoint] post-data)
                 (_create-job0 'herbie-command
                               (create-job 'cost
                                           (get-test post-data)
                                           #:seed #f
                                           #:pcontext #f
                                           #:profile? #f
                                           #:timeline-disabled? #f)))

;; TODO combine mathjs and translate endpoints and unify json output
(define-endpoint ([mathjs-endpoint start-mathjs-endpoint] post-data)
                 (_create-job0 'translate (translate-job (hash-ref post-data 'formula) 'mathjs)))

(define-endpoint ([translate-endpoint start-translate-endpoint] post-data)
                 (define target-lang (hash-ref post-data 'language))
                 (define formula (hash-ref post-data 'formula))
                 (eprintf "Translating ~a to language: ~a...\n" formula target-lang)
                 (_create-job0 'translate (translate-job formula target-lang)))

(define (run-demo #:quiet [quiet? #f]
                  #:threads [threads #f]
                  #:browser [browser? #t]
                  #:output output
                  #:demo? demo?
                  #:prefix prefix
                  #:log log
                  #:port port
                  #:public? public)
  (*demo?* demo?)
  (*demo-output* output)
  (*demo-prefix* prefix)
  (*demo-log* log)
  (start-job-server threads)

  (unless quiet?
    (eprintf "Herbie ~a with seed ~a\n" *herbie-version* (get-seed))
    (eprintf "Find help on https://herbie.uwplse.org/, exit with Ctrl-C\n"))

  (serve/servlet dispatch
                 #:listen-ip (if public #f "127.0.0.1")
                 #:port port
                 #:safety-limits
                 (make-safety-limits #:max-request-body-length
                                     (* 5 1024 1024)) ; 5 mb body size for det44 bench mark.
                 #:servlet-current-directory (current-directory)
                 #:manager (create-none-manager #f)
                 #:command-line? true
                 #:launch-browser? (and browser? (not quiet?))
                 #:banner? (not quiet?)
                 #:servlets-root (web-resource)
                 #:server-root-path (web-resource)
                 #:servlet-path "/"
                 #:servlet-regexp #rx""
                 #:extra-files-paths (filter identity (list (web-resource) (*demo-output*)))
                 #:log-file (*demo-log*)
                 #:file-not-found-responder (gen-file-not-found-responder (web-resource "404.html"))))

(module+ main
  (run-demo #t))
