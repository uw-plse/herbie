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
              ["api/run.rkt" (make-report rerun-report)]
              ["api/shell.rkt" (run-shell run-improve)])

(define (string->thread-count th)
  (match th
    ["no" #f]
    ["yes" (max (- (processor-count) 1) 1)]
    [(app string->number (? positive-integer? x)) x]
    [_ (raise-herbie-error "Invalid thread count `~a`" th #:url "options.html")]))

(define (string->flag s)
  (match (string-split s ":")
    [(list (app string->symbol category) (app string->symbol flag))
     #:when (set-member? (dict-ref all-flags category '()) flag)
     (list category flag)]
    [_ (raise-herbie-error "Invalid flag `~a`" s #:url "options.html")]))

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
  (set-seed! (random 1 (expt 2 31)))

  (multi-command-line
   #:program "herbie"
   #:version (format "Herbie ~a" *herbie-version*)
   #:once-each
   [("--timeout")
    s
    ("Timeout for each test (in seconds)." (format "[Default: ~a seconds]" (/ (*timeout*) 1000)))
    (*timeout* (* 1000 (string->number s)))]
   [("--seed")
    int
    ("The random seed to use in point generation." "[Default: random]")
    (define given-seed (read (open-input-string int)))
    (when given-seed
      (set-seed! given-seed))]
   [("--threads")
    num
    "How many jobs to run in parallel: Processor count is the default."
    (set! threads (string->thread-count num))]
   [("--platform")
    platform
    ("The platform to use during improvement" "[Default: default]")
    (activate-platform! (string->symbol platform))]
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
   [("--profile") "Whether to profile each run (no-op, always on)" (void)]
   #:multi [("--plugin")
            path
            ("Path to a Herbie plugin." "Allows for dynamic loading of \"loose\" plugins.")
            (dynamic-require path #f)
            (*loose-plugins* (cons path (*loose-plugins*)))]
   [("-o" "--disable")
    flag
    ("Disable a search flag (formatted category:name)."
     "See `+o/--enable` for the full list of search flags.")
    (apply disable-flag! (string->flag flag))]
   [("+o" "--enable")
    flag
    ("Enable a search flag (formatted category:name)."
     (format "Description of each search flag: https://herbie.uwplse.org/doc/~a/options.html."
             *herbie-version*)
     (apply string-append
            "\n" ;; 5 spaces is the padding inserted by `command-line`
            (map (curry format "     ~a\n") (default-flags->table))))
    (apply enable-flag! (string->flag flag))]
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
    #:args (input output)
    (run-improve input output #:threads threads)]
   [report
    "Run Herbie on an FPCore file, producing an HTML report"
    #:args (input output)
    (make-report (list input) #:dir output #:threads threads)]
   [reproduce
    "Rerun an HTML report"
    #:args (input output)
    (rerun-report input #:dir output #:threads threads)]
   #:args files
   (match files
     ['()
      (raise-herbie-error "Please specify a Herbie tool like `racket -l herbie web`"
                          #:url "options.html")]
     [(cons tool _)
      (raise-herbie-error "Unknown tool `~a`. List available tools with `racket -l herbie -- --help`"
                          tool
                          #:url "options.html")])))
