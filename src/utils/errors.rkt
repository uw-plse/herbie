#lang racket
(require "../config.rkt")
(provide raise-herbie-error
         raise-herbie-syntax-error
         raise-herbie-sampling-error
         raise-herbie-missing-error
         syntax->error-format-string
         exception->datum
         herbie-error->string
         herbie-error-url
         (struct-out exn:fail:user:herbie)
         (struct-out exn:fail:user:herbie:syntax)
         (struct-out exn:fail:user:herbie:sampling)
         (struct-out exn:fail:user:herbie:missing)
         warn
         warning-log
         *warnings-disabled*)

(struct exn:fail:user:herbie exn:fail:user (url) #:extra-constructor-name make-exn:fail:user:herbie)

(struct exn:fail:user:herbie:syntax exn:fail:user:herbie (locations)
  #:extra-constructor-name make-exn:fail:user:herbie:syntax)

(struct exn:fail:user:herbie:sampling exn:fail:user:herbie ()
  #:extra-constructor-name make-exn:fail:user:herbie:sampling)

(struct exn:fail:user:herbie:missing exn:fail:user:herbie ()
  #:extra-constructor-name make-exn:fail:user:herbie:missing)

(define (raise-herbie-error message #:url [url #f] . args)
  (raise (make-exn:fail:user:herbie (apply format message args) (current-continuation-marks) url)))

(define (raise-herbie-syntax-error message
                                   #:url [url "faq.html#invalid-syntax"]
                                   #:locations [locations '()]
                                   . args)
  (raise (make-exn:fail:user:herbie:syntax (apply format message args)
                                           (current-continuation-marks)
                                           url
                                           locations)))

(define (raise-herbie-sampling-error message #:url [url #f] . args)
  (raise
   (make-exn:fail:user:herbie:sampling (apply format message args) (current-continuation-marks) url)))

(define (raise-herbie-missing-error message #:url [url #f] . args)
  (raise
   (make-exn:fail:user:herbie:missing (apply format message args) (current-continuation-marks) url)))

(define (herbie-error-url exn)
  (format "https://herbie.uwplse.org/doc/~a/~a" *herbie-version* (exn:fail:user:herbie-url exn)))

(define (syntax->error-format-string stx)
  (define file
    (if (path? (syntax-source stx))
        (let-values ([(base name dir?) (split-path (syntax-source stx))])
          (path->string name))
        (syntax-source stx)))
  (format "~a:~a:~a: ~~a"
          file
          (or (syntax-line stx) "")
          (or (syntax-column stx) (syntax-position stx))))

(define (traceback->datum exn)
  (define ctx (continuation-mark-set->context (exn-continuation-marks exn)))
  (for/list ([(name loc) (in-dict ctx)])
    (define name* (or name "(unnamed)"))
    (match loc
      [(srcloc file line col _ _) (cons name* (list file line col))]
      [#f (cons name* #f)])))

(define (syntax-locations->datum exn)
  (for/list ([(stx msg) (in-dict (exn:fail:user:herbie:syntax-locations exn))])
    (list msg (syntax-source stx) (syntax-line stx) (syntax-column stx) (syntax-position stx))))

(define (exception->datum exn)
  (match exn
    [(? exn:fail:user:herbie:missing?)
     (list 'exn 'missing (exn-message exn) (herbie-error-url exn) #f (traceback->datum exn))]
    [(? exn:fail:user:herbie:sampling?)
     (list 'exn 'sampling (exn-message exn) (herbie-error-url exn) #f (traceback->datum exn))]
    [(? exn:fail:user:herbie:syntax?)
     (list 'exn
           'syntax
           (exn-message exn)
           (herbie-error-url exn)
           (syntax-locations->datum exn)
           (traceback->datum exn))]
    [(? exn:fail:user:herbie?)
     (list 'exn 'herbie (exn-message exn) (herbie-error-url exn) '() (traceback->datum exn))]
    [(? exn?) (list 'exn #f (exn-message exn) #f '() (traceback->datum exn))]))

(define (herbie-error->string err)
  (call-with-output-string
   (λ (p)
     (match err
       [(exn:fail:user:herbie:syntax message marks url locations)
        (fprintf p "~a\n" message)
        (for ([(stx message) (in-dict locations)])
          (fprintf p "  ~a\n" (format (syntax->error-format-string stx) message)))
        (when url
          (fprintf p "See <https://herbie.uwplse.org/doc/~a/~a> for more.\n" *herbie-version* url))]
       [(exn:fail:user:herbie message marks url)
        (fprintf p "~a\n" message)
        (when url
          (fprintf p
                   "See <https://herbie.uwplse.org/doc/~a/~a> for more.\n"
                   *herbie-version*
                   url))]))))

(define old-error-display-handler (error-display-handler))
(error-display-handler
 (λ (message err)
   (cond
     [(exn:fail:user:herbie? err) (display (herbie-error->string err) (current-error-port))]
     [(exn:fail:read? err)
      (printf "Invalid syntax\n  ~a\nSee <https://herbie.uwplse.org/doc/~a/input.html> for more.\n"
              (exn-message err)
              *herbie-version*)]
     [else (old-error-display-handler message err)])))

(define *warnings-disabled* (make-parameter false))

(define/reset warnings (mutable-set))
(define/reset warning-log '())

(define (warn type message #:url [url #f] #:extra [extra '()] . args)
  (unless (or (*warnings-disabled*) (set-member? (warnings) type))
    (eprintf "Warning: ~a\n" (apply format message args))
    (for ([line extra])
      (eprintf "  ~a\n" line))
    (define url* (and url (format "https://herbie.uwplse.org/doc/~a/~a" *herbie-version* url)))
    (when url*
      (eprintf "See <~a> for more.\n" url*))
    (define entry (list type message args url* extra))
    (set-add! (warnings) type)
    (warning-log (cons entry (warning-log)))))
