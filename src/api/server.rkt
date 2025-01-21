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
         start-job
         wait-for-job
         start-job-server
         write-results-to-disk
         *demo?*
         *demo-output*)

(define *demo?* (make-parameter false))
(define *demo-output* (make-parameter false))

(define log-level #f)
(define (log msg . args)
  (when log-level
    (apply eprintf msg args)))

;; Job object, What herbie excepts as input for a new job.
(struct herbie-command (command test seed pcontext profile? timeline-disabled?) #:prefab)

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
        (let ([info (call-with-input-file data-file read-datafile)])
          (struct-copy report-info info [tests (cons data (report-info-tests info))]))
        (make-report-info (list data) #:seed (get-seed))))
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

;; Starts a job on the server
;; TODO contract?
(define (start-job command
                   test
                   #:seed [seed #f]
                   #:pcontext [pcontext #f]
                   #:profile? [profile? #f]
                   #:timeline-disabled? [timeline-disabled? #f])
  (define job (herbie-command command test seed pcontext profile? timeline-disabled?))
  (define job-id (compute-job-id job))
  (manager-tell 'start manager job job-id)
  (log "Job ~a, Qed up for program: ~a\n" job-id (test-name test))
  job-id)

(define (wait-for-job job-id)
  (define finished-result (manager-ask 'wait manager job-id))
  (log "Done waiting for: ~a\n" job-id)
  finished-result)

(define (manager-tell msg . args)
  (log "Telling manager: ~a, ~a.\n" msg args)
  (if manager
      (place-channel-put manager (list* msg args))
      (match msg
        ['start
         (match-define (list hash-false command job-id) args)
         (hash-set! completed-work job-id (herbie-do-server-job command job-id))])))

(define (manager-ask msg . args)
  (log "Asking manager: ~a, ~a.\n" msg args)
  (if manager
      (manager-ask-with-callback msg args)
      (match (list* msg args) ; public commands
        [(list 'wait hash-false job-id) (hash-ref completed-work job-id)]
        [(list 'result job-id) (hash-ref completed-work job-id #f)]
        [(list 'timeline job-id) (hash-ref completed-work job-id #f)]
        [(list 'check job-id) (and (hash-ref completed-work job-id #f) job-id)]
        [(list 'count) (list 0 0)]
        [(list 'improve)
         (for/list ([(job-id result) (in-hash completed-work)]
                    #:when (equal? (hash-ref result 'command) "improve"))
           (get-table-data-from-hash result (make-path job-id)))])))

(define (get-json-converter command)
  (match (herbie-command-command command)
    ['alternatives make-alternatives-result]
    ['cost make-cost-result]
    ['errors make-error-result]
    ['evaluate make-calculate-result]
    ['exacts make-exacts-result]
    ['explanations make-explanation-result]
    ['improve make-improve-result]
    ['local-error make-local-error-result]
    ['sample make-sample-result]
    [_ (error 'compute-result "unknown command ~a" command)]))

(define (herbie-do-server-job command job-id)
  (define herbie-result (wrapper-run-herbie command job-id))
  (define basic-output ((get-json-converter command) herbie-result job-id))
  ;; Add default fields that all commands have
  (hash-set* basic-output
             'command
             (~a (herbie-command-command command))
             'job
             job-id
             'path
             (make-path job-id)))

(define completed-work (make-hash))

(define (manager-ask-with-callback msg args)
  (define-values (a b) (place-channel))
  (place-channel-put manager (list* msg b args))
  (place-channel-get a))

(define (is-server-up)
  (if manager
      (not (sync/timeout 0 manager-dead-event))
      #t))

;; Start the job server
;; worker-cap: `false` or `no` to not use Racket `place` best used for
;; debugging, specific yes to use the number of cores on your system as the
;; worker cap or specif the number of workers you would like to use
;; logging: Set to #f as default. Set to #t to print what the server is doing
;; to standard error.
(define (start-job-server worker-cap #:logging [set-logging #f])
  (set! log-level set-logging)
  (when worker-cap
    (define r (make-manager worker-cap))
    (set! manager-dead-event (place-dead-evt r))
    (set! manager r)))

(define manager #f)
(define manager-dead-event #f)

(define (compute-job-id job-info)
  (sha1 (open-input-string (~s job-info))))

(define (wrapper-run-herbie cmd job-id)
  (log "Started ~a job (~a): ~a\n"
       (herbie-command-command cmd)
       job-id
       (test-name (herbie-command-test cmd)))
  (begin0 (run-herbie (herbie-command-command cmd)
                      (herbie-command-test cmd)
                      #:seed (herbie-command-seed cmd)
                      #:pcontext (herbie-command-pcontext cmd)
                      #:profile? (herbie-command-profile? cmd)
                      #:timeline-disabled? (herbie-command-timeline-disabled? cmd))
    (log "Completed ~a job (~a)\n" (herbie-command-command cmd) job-id)))

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name
                          (parameterize ([params fresh] ...)
                            body ...))))]))

(struct work-item (command id))

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
   (when (eq? worker-count #t)
     (set! worker-count (processor-count)))
   (for ([i (in-range worker-count)])
     (hash-set! waiting-workers i (make-worker i)))
   (log "~a workers ready.\n" (hash-count waiting-workers))
   (define waiting (make-hash))
   (define job-queue (list))
   (log "Manager waiting to assign work.\n")
   (for ([i (in-naturals)])
     (match (place-channel-get ch)
       [(list 'start self command job-id)
        ; Check if the work has been completed already if not assign the work.
        (if (hash-has-key? completed-work job-id)
            (place-channel-put self (list 'send job-id (hash-ref completed-work job-id)))
            (place-channel-put self (list 'queue self job-id command)))]
       [(list 'queue self job-id command)
        (set! job-queue (append job-queue (list (work-item command job-id))))
        (place-channel-put self (list 'assign self))]
       [(list 'assign self)
        (define reassigned (make-hash))
        (for ([(wid worker) (in-hash waiting-workers)]
              [job (in-list job-queue)])
          (log "Starting worker [~a] on [~a].\n"
               (work-item-id job)
               (test-name (herbie-command-test (work-item-command job))))
          ; Check if the job is already in progress.
          (unless (hash-has-key? current-jobs (work-item-id job))
            (hash-set! current-jobs (work-item-id job) wid)
            (place-channel-put worker (list 'apply self (work-item-command job) (work-item-id job)))
            (hash-set! reassigned wid worker)
            (hash-set! busy-workers wid worker)))
        ; remove X many jobs from the Q and update waiting-workers
        (for ([(wid worker) (in-hash reassigned)])
          (hash-remove! waiting-workers wid)
          (set! job-queue (cdr job-queue)))]
       ; Job is finished save work and free worker. Move work to 'send state.
       [(list 'finished self wid job-id result)
        (log "Job ~a finished, saving result.\n" job-id)
        (hash-set! completed-work job-id result)

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
        (unless (false? result)
          (log "Done waiting for job: ~a\n" job-id)
          ; we have a result to send.
          (place-channel-put self (list 'send job-id result)))]
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
        (place-channel-put handler (and (hash-has-key? completed-work job-id) job-id))]
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
       [(list 'apply manager command job-id)
        (set! timeline (*timeline*))
        (set! current-job-id job-id)
        (log "[~a] working on [~a].\n" job-id (test-name (herbie-command-test command)))
        (thread-send worker-thread (work manager worker-id job-id command))]
       [(list 'timeline handler)
        (log "Timeline requested from worker[~a] for job ~a\n" worker-id current-job-id)
        (place-channel-put handler (reverse (unbox timeline)))]))))

(struct work (manager worker-id job-id job))

(define (run-job job-info)
  (match-define (work manager worker-id job-id command) job-info)
  (log "run-job: ~a, ~a\n" worker-id job-id)
  (define out-result (herbie-do-server-job command job-id))
  (log "Job: ~a finished, returning work to manager\n" job-id)
  (place-channel-put manager (list 'finished manager worker-id job-id out-result)))

(define (make-explanation-result herbie-result job-id)
  (hasheq 'explanation (job-result-backend herbie-result)))

(define (make-local-error-result herbie-result job-id)
  (hasheq 'tree (job-result-backend herbie-result)))

(define (make-sample-result herbie-result job-id)
  (define test (job-result-test herbie-result))
  (define pctx (job-result-backend herbie-result))
  (define repr (context-repr (test-context test)))
  (hasheq 'points (pcontext->json pctx repr)))

(define (make-calculate-result herbie-result job-id)
  (hasheq 'points (job-result-backend herbie-result)))

(define (make-cost-result herbie-result job-id)
  (hasheq 'cost (job-result-backend herbie-result)))

(define (make-error-result herbie-result job-id)
  (define errs
    (for/list ([pt&err (job-result-backend herbie-result)])
      (define pt (first pt&err))
      (define err (second pt&err))
      (list pt (format-bits (ulps->bits err)))))
  (hasheq 'points errs))

(define (make-exacts-result herbie-result job-id)
  (hasheq 'points (job-result-backend herbie-result)))

(define (make-improve-result herbie-result job-id)
  (define test (job-result-test herbie-result))
  (define ctx (context->json (test-context test)))
  (define backend (job-result-backend herbie-result))
  (define job-time (job-result-time herbie-result))
  (define warnings (job-result-warnings herbie-result))
  (define timeline (job-result-timeline herbie-result))
  (define profile (job-result-profile herbie-result))

  (define repr (test-output-repr test))
  (define backend-hash
    (match (job-result-status herbie-result)
      ['success (backend-improve-result-hash-table backend repr test)]
      ['timeout #f]
      ['failure (exception->datum backend)]))

  (hasheq 'status
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
          'profile
          profile
          'backend
          backend-hash))

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

(define (make-alternatives-result herbie-result job-id)

  (define test (job-result-test herbie-result))
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
      (define os (open-output-string))
      (parameterize ([current-output-port os])
        (write-xexpr
         `(div ([id "history"])
               (ol ,@(render-history altn processed-pcontext test-pcontext (test-context test)))))
        (get-output-string os))))
  (define derivations
    (for/list ([altn altns])
      (render-json altn processed-pcontext test-pcontext (test-context test))))
  (hasheq 'alternatives
          fpcores
          'histories
          histories
          'derivations
          derivations
          'splitpoints
          splitpoints))
