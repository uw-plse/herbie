#lang racket

(require "../src/utils/common.rkt"
         "../src/core/points.rkt"
         "../src/syntax/load-plugin.rkt"
         "../src/utils/alternative.rkt"
         "../src/api/sandbox.rkt"
         "../src/syntax/read.rkt"
         "../src/syntax/types.rkt"
         "../src/syntax/platform.rkt")

(define *precision* (make-parameter #f))

(define (test-successful? test input-bits target-bits output-bits)
  (match* ((test-output test) (test-expected test))
    [(_ #f) #t]
    [(_ (? number? n)) (>= n output-bits)]
    [('() #t) (>= input-bits output-bits)]
    [(_ #t) (>= target-bits (- output-bits 1))]))

(define (override-test-precision the-test repr)
  (struct-copy test
               the-test
               [output-repr-name (representation-name repr)]
               [var-repr-names
                (for/list ([var (in-list (test-vars the-test))])
                  (cons var (representation-name repr)))]))

(define (run-tests . bench-dirs)
  (define default-precision
    (if (*precision*)
        (representation-name (*precision*))
        (*default-precision*)))
  (define tests
    (parameterize ([*default-precision* default-precision])
      (append-map load-tests bench-dirs)))
  (define seed (pseudo-random-generator->vector (current-pseudo-random-generator)))
  (printf "Running Herbie on ~a tests, seed: ~a\n" (length tests) seed)
  (for/and ([the-test tests]
            [i (in-naturals)])
    (printf "~a/~a\t" (~a (+ 1 i) #:width 3 #:align 'right) (length tests))
    (define the-test*
      (if (*precision*)
          (override-test-precision the-test (*precision*))
          the-test))
    (define result (run-herbie 'improve the-test* #:seed seed))
    (match-define (job-result _ test status time timeline warnings backend) result)
    (match status
      ['success
       (match-define (improve-result preprocess pctxs start targets end bogosity) backend)
       (match-define (alt-analysis start-alt _ start-error) start)
       (match-define (alt-analysis end-alt _ end-error) (first end))

       ;; Pick lowest target from all target
       (define target-error
         ; If the list is empty, return false
         (if (empty? targets)
             #f
             (argmin errors-score (map alt-analysis-test-errors targets))))

       (printf "[ ~as]   ~a→~a\t~a\n"
               (~r (/ time 1000) #:min-width 7 #:precision '(= 3))
               (~r (errors-score start-error) #:min-width 2 #:precision 0)
               (~r (errors-score end-error) #:min-width 2 #:precision 0)
               (test-name test))

       (define success?
         (test-successful? test
                           (errors-score start-error)
                           (if target-error
                               (errors-score target-error)
                               #f)
                           (errors-score end-error)))

       (when (not success?)
         (printf "\nInput (~a bits):\n" (errors-score start-error))
         (pretty-print (alt-expr start-alt) (current-output-port) 1)
         (printf "\nOutput (~a bits):\n" (errors-score end-error))
         (pretty-print (alt-expr end-alt) (current-output-port) 1)

         (when target-error
           (printf "\nTarget (~a bits):\n" (errors-score target-error))
           ;; internal tool so okay
           (pretty-print (list-ref (test-output test) 0) (current-output-port) 1)))

       success?]
      ['failure
       (define exn backend)
       (printf "[  CRASH  ]\t\t~a\n" (test-name test))
       ((error-display-handler) (exn-message exn) exn)
       #f]
      ['timeout
       (printf "[  TIMEOUT]\t\t~a\n" (test-name test))
       #f])))

(module+ main
  ;; Load all the plugins
  (load-herbie-plugins)

  (define seed (random 1 (expt 2 31)))
  (set-seed! seed)

  (command-line #:program "ci.rkt"
                #:multi [("--plugin")
                         path
                         "Which additional Herbie plugins to use"
                         (dynamic-require path #f)
                         (*loose-plugins* (cons path (*loose-plugins*)))]
                #:once-each
                [("--seed")
                 rs
                 "The random seed to use in point generation. If false (#f), a random seed is used'"
                 (define given-seed (read (open-input-string rs)))
                 (when given-seed
                   (set-seed! given-seed))]
                [("--platform")
                 platform
                 "Which platform to use for tests"
                 (*platform-name* (string->symbol platform))
                 (*active-platform* (get-platform (*platform-name*)))
                 (activate-platform! (*active-platform*))]
                [("--precision")
                 prec
                 "Which precision to use for tests"
                 (*precision* (get-representation (string->symbol prec)))]
                [("--num-iters")
                 num
                 "The number of iterations to use for the main loop"
                 (*num-iterations* (string->number num))]
                #:args bench-dir
                (exit (if (apply run-tests bench-dir) 0 1))))
