#lang racket

(require openssl/sha1)
(require (only-in xml write-xexpr))

(require "sandbox.rkt"
         "../core/points.rkt"
         "../reports/history.rkt"
         "../reports/plot.rkt"
         "../reports/common.rkt"
         "../syntax/types.rkt"
         "../syntax/read.rkt"
         "../syntax/load-plugin.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../reports/pages.rkt"
         "../reports/core2mathjs.rkt"
         "datafile.rkt"
         (submod "../utils/timeline.rkt" debug))

(provide make-path
         get-improve-table-data
         make-improve-result
         server-check-on
         get-results-for
         get-timeline-for
         job-count
         is-server-up
         create-job
         start-job
         wait-for-job
         start-job-server
         write-results-to-disk
         *demo?*
         *demo-output*
         _create-job0
         translate-job)

(define *demo?* (make-parameter false))
(define *demo-output* (make-parameter false))

; verbose logging for debugging
(define verbose #t) ; Maybe change to log-level and use 'verbose?
(define (log msg . args)
  (when verbose
    (apply eprintf msg args)))

;; Job object, What herbie excepts as input for a new job.
(struct herbie-command (command test seed pcontext profile? timeline-disabled?) #:prefab)

; action-type 'herbie-command or 'translation
(struct server-action (action-type associated-type) #:prefab)

(struct translate-job (fpcore to-language) #:prefab)

(define (_create-job0 server-action-type associated-type)
  (match server-action-type
    ['herbie-command (eprintf "creating herbie command\n")]
    ['translate (eprintf "creating translation\n")])
  (server-action server-action-type associated-type))

;; Creates a command object to be passed to start-job server.
;; TODO contract?
(define (create-job command
                    test
                    #:seed [seed #f]
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline-disabled? [timeline-disabled? #f])
  (herbie-command command test seed pcontext profile? timeline-disabled?))

(define (write-results-to-disk result-hash path)
  (make-directory (build-path (*demo-output*) path))
  (for ([page (all-pages result-hash)])
    (call-with-output-file (build-path (*demo-output*) path page)
                           (λ (out)
                             (with-handlers ([exn:fail? (page-error-handler result-hash page out)])
                               (make-page page out result-hash (*demo-output*) #f)))))
  (define link (path-element->string (last (explode-path path))))
  (define data (get-table-data-from-hash result-hash link))
  (define data-file (build-path (*demo-output*) "results.json"))
  (define html-file (build-path (*demo-output*) "index.html"))
  (define info
    (if (file-exists? data-file)
        (let ([info (read-datafile data-file)])
          (struct-copy report-info info [tests (cons data (report-info-tests info))]))
        (make-report-info (list data) #:seed (get-seed) #:note (if (*demo?*) "Web demo results" ""))))
  (define tmp-file (build-path (*demo-output*) "results.tmp"))
  (write-datafile tmp-file info)
  (rename-file-or-directory tmp-file data-file #t)
  (copy-file (web-resource "report.html") html-file #t))

; computes the path used for server URLs
(define (make-path id)
  (format "~a.~a" id *herbie-commit*))

; Returns #f is now job exsist for the given job-id
(define (get-results-for job-id)
  (log "Getting result for job: ~a.\n" job-id)
  (manager-ask 'result job-id))

; Invlaid to ask for timeline of a 'translate job.
(define (get-timeline-for job-id)
  (log "Getting timeline for job: ~a.\n" job-id)
  (manager-ask 'timeline job-id))

; Returns #f if there is no job returns the job-id if there is a completed job.
(define (server-check-on job-id)
  (log "Checking on: ~a.\n" job-id)
  (manager-ask 'check job-id))

(define (get-improve-table-data)
  (log "Getting improve results.\n")
  (manager-ask 'improve))

(define (job-count)
  (define job-list (manager-ask 'count))
  (log "Currently ~a jobs in progress, ~a jobs in queue.\n" (first job-list) (second job-list))
  (apply + job-list))

;; Starts a job for a given command object|
(define (start-job action)
  (match (server-action-action-type action)
    ['herbie-command
     (define command (server-action-associated-type action))
     (define job-id (compute-job-id command))
     (manager-tell 'start manager action job-id)
     (log "Qed up herbie command ~a for job ~a program: ~a\n"
          (herbie-command-command command)
          job-id
          (test-name (herbie-command-test command)))
     job-id]
    ['translate
     (match-define (translate-job fpcore to-language) (server-action-associated-type action))
     (log "Qed up translation Job: ~a to ~a.\n" fpcore to-language)
     (define job-hash (compute-job-id (translate-job fpcore to-language)))
     (manager-tell 'start manager action job-hash)
     job-hash]))

(define (wait-for-job job-id)
  (define finished-result (manager-ask 'wait manager job-id))
  (log "Done waiting for: ~a\n" job-id)
  finished-result)

(define (manager-tell msg . args)
  (log "Telling manager: ~a, ~a.\n" msg args)
  (if manager
      (place-channel-put manager (list* msg args))
      (match (list* msg args)
        [(list 'start hash-false action job-id)
         (match (server-action-action-type action)
           ['herbie-command
            (define command (server-action-associated-type action))
            (hash-set! completed-work job-id (herbie-do-server-job command job-id))]
           ['translate
            (match-define (translate-job fpcore to-language) (server-action-associated-type action))
            (define result (do-translation->json fpcore to-language))
            (log "Converted Expression: \n~a...\n" result)
            (hash-set! completed-translations job-id result)])])))

; Translate and return the expected Odyssey json format.
(define (do-translation->json formula to-language)
  (eprintf "formual: ~a\n" formula)
  (define input (read (open-input-string formula)))
  (match to-language
    ['mathjs (hasheq 'mathjs (core->mathjs input))]
    ["python" (hasheq 'result (core->python input "expr") 'language to-language)]
    ["c" (hasheq 'result (core->c input "expr") 'language to-language)]
    ["fortran" (hasheq 'result (core->fortran input "expr") 'language to-language)]
    ["java" (hasheq 'result (core->java input "expr") 'language to-language)]
    ["julia" (hasheq 'result (core->julia input "expr") 'language to-language)]
    ["matlab" (hasheq 'result (core->matlab input "expr") 'language to-language)]
    ["wls" (hasheq 'result (core->wls input "expr") 'language to-language)]
    ["tex" (hasheq 'result (core->tex input "expr") 'language to-language)]
    ["js" (hasheq 'result (core->js input "expr") 'language to-language)]
    [_ (error "Unsupported target language:" to-language)]))

(define (manager-ask msg . args)
  (log "Asking manager: ~a, ~a.\n" msg args)
  (if manager
      (manager-ask-with-callback msg args)
      (match (list* msg args) ; public commands
        [(list 'wait hash-false job-id)
         (define result (hash-ref completed-work job-id #f))
         (match result
           [#f
            (match (hash-ref completed-translations job-id #f)
              [#f (log "Job ~a not complete\n" job-id)]
              [t
               (log "Done waiting for translation: ~a\n" job-id)
               t])]
           [result
            (log "Done waiting for job: ~a\n" job-id)
            result])]
        [(list 'result job-id) (hash-ref completed-work job-id #f)]
        [(list 'timeline job-id) (hash-ref completed-work job-id #f)]
        [(list 'check job-id) (if (hash-ref completed-work job-id #f) job-id #f)]
        [(list 'count) (list 0 0)]
        [(list 'improve)
         (for/list ([(job-id result) (in-hash completed-work)]
                    #:when (equal? (hash-ref result 'command) "improve"))
           (get-table-data-from-hash result (make-path job-id)))])))

(define (herbie-do-server-job command job-id)
  (define herbie-result (wrapper-run-herbie command job-id))
  (match-define (job-result kind test status time _ _ backend) herbie-result)
  (define out-result
    (match kind
      ['alternatives (make-alternatives-result herbie-result test job-id)]
      ['evaluate (make-calculate-result herbie-result job-id)]
      ['cost (make-cost-result herbie-result job-id)]
      ['errors (make-error-result herbie-result job-id)]
      ['exacts (make-exacts-result herbie-result job-id)]
      ['improve (make-improve-result herbie-result test job-id)]
      ['local-error (make-local-error-result herbie-result job-id)]
      ['explanations (make-explanation-result herbie-result job-id)]
      ['sample (make-sample-result herbie-result test job-id)]
      [_ (error 'compute-result "unknown command ~a" kind)]))
  out-result)

;; These are split for the 'improve call.
(define completed-work (make-hash))
(define completed-translations (make-hash))

(define (manager-ask-with-callback msg args)
  (define-values (a b) (place-channel))
  (place-channel-put manager (list* msg b args))
  (place-channel-get a))

(define (is-server-up)
  (if manager
      (not (sync/timeout 0 manager-dead-event))
      #t))

(define (start-job-server job-cap)
  (when job-cap
    (define r (make-manager job-cap))
    (set! manager-dead-event (place-dead-evt r))
    (set! manager r)))

(define manager #f)
(define manager-dead-event #f)

(define (get-command herbie-result)
  ; force symbol type to string.
  ; This is a HACK to fix JSON parsing errors that may or may not still happen.
  (~s (job-result-command herbie-result)))

(define (compute-job-id job-info)
  (sha1 (open-input-string (~s job-info))))

(define (wrapper-run-herbie cmd job-id)
  (print-job-message (herbie-command-command cmd) job-id (test-name (herbie-command-test cmd)))
  (define result
    (run-herbie (herbie-command-command cmd)
                (herbie-command-test cmd)
                #:seed (herbie-command-seed cmd)
                #:pcontext (herbie-command-pcontext cmd)
                #:profile? (herbie-command-profile? cmd)
                #:timeline-disabled? (herbie-command-timeline-disabled? cmd)))
  (eprintf "Herbie completed job: ~a\n" job-id)
  result)

(define (print-job-message command job-id job-str)
  (define job-label
    (match command
      ['alternatives "Alternatives"]
      ['evaluate "Evaluation"]
      ['cost "Computing"]
      ['errors "Analyze"]
      ['exacts "Ground truth"]
      ['improve "Improve"]
      ['local-error "Local error"]
      ['explanations "Explanations"]
      ['sample "Sampling"]
      [_ (error 'compute-result "unknown command ~a" command)]))
  (eprintf "~a Job ~a started:\n  ~a ~a...\n" job-label (symbol->string command) job-id job-str))

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name
                          (parameterize ([params fresh] ...)
                            body ...))))]))

(define (make-manager worker-count)
  (place/context*
   ch
   #:parameters (*flags* *num-iterations*
                         *num-points*
                         *timeout*
                         *reeval-pts*
                         *node-limit*
                         *max-find-range-depth*
                         *pareto-mode*
                         *platform-name*
                         *loose-plugins*)
   (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
     (load-herbie-plugins))
   ; not sure if the above code is actaully needed.
   (define busy-workers (make-hash))
   (define waiting-workers (make-hash))
   (define current-jobs (make-hash))
   (for ([i (in-range worker-count)])
     (hash-set! waiting-workers i (make-worker i)))
   (log "~a workers ready.\n" (hash-count waiting-workers))
   (define waiting (make-hash))
   (define job-queue (list))
   (log "Manager waiting to assign work.\n")
   (for ([i (in-naturals)])
     ;  (eprintf "manager msg ~a handled\n" i)
     (match (place-channel-get ch)
       [(list 'start self action job-id)
        (match (server-action-action-type action)
          ; Check if the work has been completed already if not assign the work.
          ['herbie-command ; Check herbie jobs
           (if (hash-has-key? completed-work job-id)
               (place-channel-put self (list 'send job-id (hash-ref completed-work job-id)))
               (place-channel-put self (list 'queue self job-id action)))]
          ['translate ; Check translations
           (if (hash-has-key? completed-translations job-id)
               (place-channel-put self (list 'send job-id (hash-ref completed-translations job-id)))
               (place-channel-put self (list 'queue self job-id action)))])]
       [(list 'queue self job-id action)
        (log "Queuing: ~a\n" job-id)
        (set! job-queue (append job-queue (list (list action job-id))))
        (place-channel-put self (list 'assign self))]
       [(list 'assign self)
        (log "Assigning work\n")
        (define reassigned (make-hash))
        (for ([(wid worker) (in-hash waiting-workers)]
              [work (in-list job-queue)])
          (match-define (list action job-id) work)
          (log "Starting worker ~a\n" job-id)
          ; Check if the job is already in progress.
          (unless (hash-has-key? current-jobs job-id)
            (hash-set! current-jobs job-id wid)
            (place-channel-put worker (list 'apply self action job-id))
            (hash-set! reassigned wid worker)
            (hash-set! busy-workers wid worker)))
        ; remove X many jobs from the Q and update waiting-workers
        (for ([(wid worker) (in-hash reassigned)])
          (hash-remove! waiting-workers wid)
          (set! job-queue (cdr job-queue)))]
       ; Job is finished save work and free worker. Move work to 'send state.
       [(list 'finished self wid job-id type result)
        (log "Job ~a finished, saving result.\n" job-id)
        (match type
          ['herbie-command (hash-set! completed-work job-id result)]
          ['translate (hash-set! completed-translations job-id result)])

        ; move worker to waiting list
        (hash-remove! current-jobs job-id)
        (hash-set! waiting-workers wid (hash-ref busy-workers wid))
        (hash-remove! busy-workers wid)

        (log "waiting job ~a completed\n" job-id)
        (place-channel-put self (list 'send job-id result))
        (place-channel-put self (list 'assign self))]
       [(list 'wait handler self job-id)
        (log "Waiting for job: ~a\n" job-id)
        ; first we add the handler to the wait list.
        (hash-update! waiting job-id (curry append (list handler)) '())
        (define result (hash-ref completed-work job-id #f))
        ; check if the job is completed or not.
        (match result
          [#f
           (match (hash-ref completed-translations job-id #f)
             [#f (log "Translation not found\n")]
             [t
              (log "Done waiting for translation job: ~a\n" job-id)
              (place-channel-put self (list 'send job-id t))])]
          [result
           (log "Done waiting for job: ~a\n" job-id)
           ; we have a result to send.
           (place-channel-put self (list 'send job-id result))])]
       [(list 'send job-id result)
        (log "Sending result for ~a.\n" job-id)
        (for ([handle (hash-ref waiting job-id '())])
          (place-channel-put handle result))
        (hash-remove! waiting job-id)]
       ; Get the result for the given id, return false if no work found.
       [(list 'result handler job-id) (place-channel-put handler (hash-ref completed-work job-id #f))]
       [(list 'timeline handler job-id)
        (define wid (hash-ref current-jobs job-id #f))
        (cond
          [wid
           (log "Worker[~a] working on ~a.\n" wid job-id)
           (define-values (a b) (place-channel))
           (place-channel-put (hash-ref busy-workers wid) (list 'timeline b))
           (define requested-timeline (place-channel-get a))
           (place-channel-put handler requested-timeline)]
          [else
           (log "Job complete, no timeline, send result.\n")
           (place-channel-put handler (hash-ref completed-work job-id #f))])]
       [(list 'check handler job-id)
        (place-channel-put handler (if (hash-has-key? completed-work job-id) job-id #f))]
       ; Returns the current count of working workers.
       [(list 'count handler)
        (log "Count requested\n")
        (place-channel-put handler (list (hash-count busy-workers) (length job-queue)))]
       ; Retreive the improve results for results.json
       [(list 'improve handler)
        (define improved-list
          (for/list ([(job-id result) (in-hash completed-work)]
                     #:when (equal? (hash-ref result 'command) "improve"))
            (get-table-data-from-hash result (make-path job-id))))
        (place-channel-put handler improved-list)]))))

(define (make-worker worker-id)
  (place/context*
   ch
   #:parameters (*flags* *num-iterations*
                         *num-points*
                         *timeout*
                         *reeval-pts*
                         *node-limit*
                         *max-find-range-depth*
                         *pareto-mode*
                         *platform-name*
                         *loose-plugins*)
   (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
     (load-herbie-plugins))
   (define worker-thread
     (thread (λ ()
               (let loop ([seed #f])
                 (match (thread-receive)
                   [job-info (run-job job-info)])
                 (loop seed)))))
   (define timeline #f)
   (define current-job-id #f)
   (for ([_ (in-naturals)])
     (match (place-channel-get ch)
       [(list 'apply manager action job-id)
        (set! current-job-id job-id)
        (match (server-action-action-type action)
          ['herbie-command
           (define command (server-action-associated-type action))
           (set! timeline (*timeline*))
           (log "[~a] working on [~a].\n" job-id (test-name (herbie-command-test command)))
           (thread-send worker-thread (work manager worker-id job-id action))]
          ['translate
           (log "[~a] translating...\n" job-id)
           (thread-send worker-thread (work manager worker-id job-id action))])]
       [(list 'timeline handler) ;; Ignore timelines for Translations?
        (log "Timeline requested from worker[~a] for job ~a\n" worker-id current-job-id)
        (place-channel-put handler (reverse (unbox timeline)))]))))

(struct work (manager worker-id job-id job))

(define (run-job job-info)
  (match-define (work manager worker-id job-id action) job-info)
  (match (server-action-action-type action)
    ['herbie-command
     (define command (server-action-associated-type action))
     (log "run-job: ~a, ~a\n" worker-id job-id)
     (define out-result (herbie-do-server-job command job-id))
     (log "Job: ~a finished, returning work to manager\n" job-id)
     (place-channel-put
      manager
      (list 'finished manager worker-id job-id (server-action-action-type action) out-result))]
    ['translate
     (match-define (translate-job fpcore to-language) (server-action-associated-type action))
     (log "Translate Job: ~a finished, returning work to manager\n" job-id)
     (place-channel-put manager
                        (list 'finished
                              manager
                              worker-id
                              job-id
                              (server-action-action-type action)
                              (do-translation->json fpcore to-language)))]))

(define (make-explanation-result herbie-result job-id)
  (define explanations (job-result-backend herbie-result))
  (hasheq 'command
          (get-command herbie-result)
          'explanation
          explanations
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-local-error-result herbie-result job-id)
  (hasheq 'command
          (get-command herbie-result)
          'tree
          (job-result-backend herbie-result)
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-sample-result herbie-result test job-id)
  (define pctx (job-result-backend herbie-result))
  (define repr (context-repr (test-context test)))
  (hasheq 'command
          (get-command herbie-result)
          'points
          (pcontext->json pctx repr)
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-calculate-result herbie-result job-id)
  (hasheq 'command
          (get-command herbie-result)
          'points
          (job-result-backend herbie-result)
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-cost-result herbie-result job-id)
  (hasheq 'command
          (get-command herbie-result)
          'cost
          (job-result-backend herbie-result)
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-error-result herbie-result job-id)
  (define errs
    (for/list ([pt&err (job-result-backend herbie-result)])
      (define pt (first pt&err))
      (define err (second pt&err))
      (list pt (format-bits (ulps->bits err)))))
  (hasheq 'command (get-command herbie-result) 'points errs 'job job-id 'path (make-path job-id)))

(define (make-exacts-result herbie-result job-id)
  (hasheq 'command
          (get-command herbie-result)
          'points
          (job-result-backend herbie-result)
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-improve-result herbie-result test job-id)
  (define ctx (context->json (test-context test)))
  (define backend (job-result-backend herbie-result))
  (define job-time (job-result-time herbie-result))
  (define warnings (job-result-warnings herbie-result))
  (define timeline (job-result-timeline herbie-result))

  (define repr (test-output-repr test))
  (define backend-hash
    (match (job-result-status herbie-result)
      ['success (backend-improve-result-hash-table backend repr test)]
      ['timeout #f]
      ['failure (exception->datum backend)]))

  (hasheq 'command
          (get-command herbie-result)
          'status
          (job-result-status herbie-result)
          'test
          test
          'ctx
          ctx
          'time
          job-time
          'warnings
          warnings
          'timeline
          timeline
          'backend
          backend-hash
          'job
          job-id
          'path
          (make-path job-id)))

(define (backend-improve-result-hash-table backend repr test)
  (define pcontext (improve-result-pctxs backend))

  (define preprocessing (improve-result-preprocess backend))
  (define end-hash-table (end-hash (improve-result-end backend) repr pcontext test))

  (hasheq 'preprocessing
          preprocessing
          'pctxs
          pcontext
          'start
          (improve-result-start backend)
          'target
          (improve-result-target backend)
          'end
          end-hash-table
          'bogosity
          (improve-result-bogosity backend)))

(define (end-hash end repr pcontexts test)

  (define-values (end-alts train-errors end-errors end-costs)
    (for/lists (l1 l2 l3 l4)
               ([analysis end])
               (match-define (alt-analysis alt train-errors test-errs) analysis)
               (values alt train-errors test-errs (alt-cost alt repr))))

  (define alts-histories
    (for/list ([alt end-alts])
      (render-history alt (first pcontexts) (second pcontexts) (test-context test))))
  (define vars (test-vars test))
  (define end-alt (alt-analysis-alt (car end)))
  (define splitpoints
    (for/list ([var vars])
      (define split-var? (equal? var (regime-var end-alt)))
      (if split-var?
          (for/list ([val (regime-splitpoints end-alt)])
            (real->ordinal (repr->real val repr) repr))
          '())))

  (hasheq 'end-exprs
          (map alt-expr end-alts)
          'end-histories
          alts-histories
          'end-train-scores
          train-errors
          'end-errors
          end-errors
          'end-costs
          end-costs
          'splitpoints
          splitpoints))

(define (context->json ctx)
  (hasheq 'vars (context-vars ctx) 'repr (repr->json (context-repr ctx))))

(define (repr->json repr)
  (hasheq 'name (representation-name repr) 'type (representation-type repr)))

(define (make-alternatives-result herbie-result test job-id)

  (define vars (test-vars test))
  (define repr (test-output-repr test))

  (match-define (list altns test-pcontext processed-pcontext) (job-result-backend herbie-result))
  (define splitpoints
    (for/list ([alt altns])
      (for/list ([var vars])
        (define split-var? (equal? var (regime-var alt)))
        (if split-var?
            (for/list ([val (regime-splitpoints alt)])
              (real->ordinal (repr->real val repr) repr))
            '()))))

  (define fpcores
    (for/list ([altn altns])
      (~a (program->fpcore (alt-expr altn) (test-context test)))))

  (define histories
    (for/list ([altn altns])
      (let ([os (open-output-string)])
        (parameterize ([current-output-port os])
          (write-xexpr
           `(div ([id "history"])
                 (ol ,@(render-history altn processed-pcontext test-pcontext (test-context test)))))
          (get-output-string os)))))
  (define derivations
    (for/list ([altn altns])
      (render-json altn processed-pcontext test-pcontext (test-context test))))
  (hasheq 'command
          (get-command herbie-result)
          'alternatives
          fpcores
          'histories
          histories
          'derivations
          derivations
          'splitpoints
          splitpoints
          'job
          job-id
          'path
          (make-path job-id)))
