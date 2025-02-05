#lang racket

(require racket/lazy-require)
(require "utils/common.rkt"
         "utils/multi-command-line.rkt"
         "utils/errors.rkt"
         "syntax/load-plugin.rkt"
         "syntax/platform.rkt"
         "api/sandbox.rkt")

;; Load all the plugins
(load-herbie-plugins)

(lazy-require ["api/demo.rkt" (run-demo)]
              ["api/run.rkt" (make-report rerun-report diff-report)]
              ["api/shell.rkt" (run-shell)]
              ["api/improve.rkt" (run-improve)])

(define (string->thread-count th)
  (match th
    ["no" #f]
    ["yes" (max (- (processor-count) 1) 1)]
    [else
     (match (string->number th)
       [(? positive-integer? x) x]
       [else (error 'string->thread-count "invalid thread count ~a" th)])]))

(define (string->flag s)
  (match (string-split s ":")
    [(list (app string->symbol category) (app string->symbol flag))
     (and (dict-has-key? all-flags category)
          (set-member? (dict-ref all-flags category) flag)
          (list category flag))]
    [_ #f]))

(define (default-flags->table)
  (list* (format "~a | ~a | ~a"
                 (~a "category" #:min-width 10)
                 (~a "flag" #:min-width 20)
                 (~a "default?" #:min-width 10))
         (~a "" #:min-width 44 #:pad-string "=")
         (for*/list ([(category flags) (in-hash all-flags)]
                     [flag (in-list flags)]
                     #:unless (flag-deprecated? category flag))
           (format "~a | ~a | ~a"
                   (~a category #:min-width 10)
                   (~a flag #:min-width 20)
                   (if (flag-set? category flag) "\u2714" "")))))

(module+ main
  (define quiet? #f)
  (define browser? #t)
  (define demo-output #f)
  (define demo-log #f)
  (define demo-prefix "/")
  (define demo? #f)
  (define demo-port 8000)
  (define demo-public #f)

  (define threads #f)
  (define report-note #f)
  (define timeout-set? #f)

  (define seed (random 1 (expt 2 31)))
  (set-seed! seed)

  (multi-command-line
   #:program "herbie"
   #:version (format "Herbie ~a" *herbie-version*)
   #:once-each
   [("--timeout")
    s
    ("Timeout for each test (in seconds)." (format "[Default: ~a seconds]" (/ (*timeout*) 1000)))
    (set! timeout-set? #t)
    (*timeout* (* 1000 (string->number s)))]
   [("--seed")
    int
    ("The random seed to use in point generation." "[Default: random]")
    (define given-seed (read (open-input-string int)))
    (when given-seed
      (set-seed! given-seed))]
   [("--platform")
    platform
    ("The platform to use during improvement" "[Default: default]")
    (*platform-name* (string->symbol platform))
    (*active-platform* (get-platform (*platform-name*)))
    (activate-platform! (*active-platform*))]
   [("--num-iters")
    num
    ("The number of iterations to use for the main loop. Herbie may find additional improvements
     with more iterations. Herbie slows down with more iterations."
     (format "[Default: ~a iterations]" (*num-iterations*)))
    (*num-iterations* (string->number num))]
   [("--num-points")
    num
    ("The number of points to use during sampling. Increasing the number of points may make results
     more consistent, but may slow down Herbie."
     (format "[Default: ~a points]" (*num-points*)))
    (*num-points* (string->number num))]
   [("--num-enodes")
    num
    ("The maximum number of enodes to use during egraph-based rewriting. Herbie may find additional
     improvements with a higher limit, but run time increases exponentially."
     (format "[Default: ~a enodes]" (*node-limit*)))
    (*node-limit* (string->number num))]
   [("--num-analysis")
    num
    ("The number of input analysis iterations used when searching for valid input points
     during sampling. May fix \"Cannot sample enough valid points\" but will slow."
     (format "[Default: ~a iterations]" (*max-find-range-depth*)))
    (*max-find-range-depth* (string->number num))]
   [("--no-pareto")
    ("Disables Pareto-Herbie (Pherbie). Pareto-mode performs accuracy and expression cost
     optimization and extracts multiple output expressions that are Pareto-optimal. Disabling
     this feature forces Herbie to extract a single, most-accurate output expression."
     "[Default: Pareto-Herbie enabled]")
    (*pareto-mode* #f)]
   #:multi [("--plugin")
            path
            ("Path to a Herbie plugin." "Allows for dynamic loading of \"loose\" plugins.")
            (dynamic-require path #f)
            (*loose-plugins* (cons path (*loose-plugins*)))]
   [("-o" "--disable")
    flag
    ("Disable a search flag (formatted category:name)."
     "See `+o/--enable` for the full list of search flags.")
    (define tf (string->flag flag))
    (when (not tf)
      (raise-herbie-error "Invalid flag ~a" flag #:url "options.html"))
    (apply disable-flag! tf)]
   [("+o" "--enable")
    flag
    ("Enable a search flag (formatted category:name)."
     (format "Description of each search flag: https://herbie.uwplse.org/doc/~a/options.html."
             *herbie-version*)
     (apply string-append
            "\n" ;; 5 spaces is the padding inserted by `command-line`
            (map (curry format "     ~a\n") (default-flags->table))))
    (define tf (string->flag flag))
    (when (not tf)
      (raise-herbie-error "Invalid flag ~a" flag #:url "options.html"))
    (apply enable-flag! tf)]
   #:subcommands [shell "Interact with Herbie from the shell" #:args () (run-shell)]
   [web
    "Interact with Herbie from your browser"
    #:once-each
    [("--port") port "Port to run the web shell on" (set! demo-port (string->number port))]
    [("--public") "Whether to listen on a public port (instead of localhost)" (set! demo-public #t)]
    [("--save-session")
     dir
     "The dir to place a report from submitted expressions"
     (set! demo-output dir)]
    [("--log") file "The file to write web access log to" (set! demo-log file)]
    [("--prefix") prefix "Prefix for proxying demo" (set! demo-prefix prefix)]
    [("--demo") "Run in Herbie web demo mode. Changes some text" (set! demo? true)]
    [("--quiet") "Print a smaller banner and don't start a browser." (set! quiet? true)]
    [("--threads")
     num
     "How many jobs to run in parallel: Processor count is the default."
     (set! threads (string->thread-count num))]
    [("--no-browser") "Run the web demo but don't start a browser." (set! browser? #f)]
    #:args ()
    (run-demo #:quiet quiet?
              #:threads threads
              #:browser browser?
              #:output demo-output
              #:log demo-log
              #:prefix demo-prefix
              #:demo? demo?
              #:port demo-port
              #:public? demo-public)]
   [improve
    "Run Herbie on an FPCore file, producing an FPCore file"
    #:once-each [("--threads")
                 num
                 "How many tests to run in parallel: 'yes', 'no', or a number"
                 (set! threads (string->thread-count num))]
    #:args (input output)
    (run-improve input output #:threads threads)]
   [report
    "Run Herbie on an FPCore file, producing an HTML report"
    #:once-each [("--note") note "Add a note for this run" (set! report-note note)]
    [("--threads")
     num
     "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count num))]
    [("--profile") "Whether to profile each run" (void)]
    #:args (input output)
    (make-report (list input) #:dir output #:note report-note #:threads threads)]
   [reproduce
    "Rerun an HTML report"
    #:once-each [("--note") note "Add a note for this run" (set! report-note note)]
    [("--threads")
     num
     "How many tests to run in parallel: 'yes', 'no', or a number"
     (set! threads (string->thread-count num))]
    [("--profile") "Whether to profile each run" (void)]
    #:args (input output)
    (rerun-report input #:dir output #:note report-note #:threads threads)]
   [diff "Diff two HTML reports" #:args (old new) (diff-report old new)]
   #:args files
   (match files
     ['()
      (eprintf "Please specify a Herbie tool, such as `herbie web`.\n")
      (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)]
     [(cons tool _)
      (eprintf "Unknown Herbie tool `~a`. See a list of available tools with `herbie --help`.\n" tool)
      (eprintf "See <https://herbie.uwplse.org/doc/~a/options.html> for more.\n" *herbie-version*)])))
