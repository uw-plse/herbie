#lang racket

(provide egglog-program%)

;; Track the entire Egglog program in one go by "converting" into racket based code
(define egglog-program%
  (class object%
    (super-new)

    ;; (list of exprs) to store the program
    (define program-list '())

    ;; Add an expr to the program (in REVERSE)
    (define/public (add! expr)
    ;   (set! program-list (cons expr program-list))
        (set! program-list (append program-list expr))
      (void))

    ;; Public method to get the program list
    (define/public (get-current-program-list)
      program-list)

    ;; Get program as (list of exprs) in the ACTUAL order
    (define/public (get-actual-program)
    ;   (reverse program-list)
        program-list)
      
    ;; High-level function that writes the program to a file, runs it then returns output
    (define/public (process-egglog)
        (define curr-program (get-actual-program))

        ; (printf "FINAL program ~a\n\n" curr-program)

        (define egglog-file-path
            (let ([temp-file (make-temporary-file "program-to-egglog-~a.egg")])
            (with-output-to-file temp-file #:exists 'replace (lambda () (for-each writeln curr-program)))
            temp-file))

        (define egglog-path
            (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

        (define stdout-port (open-output-string))
        (define stderr-port (open-output-string))

        (define old-error-port (current-error-port))

        ;; Run egglog and capture output
        (parameterize ([current-output-port stdout-port]
                        [current-error-port stderr-port])
            (unless (system (format "~a ~a" egglog-path egglog-file-path))
            (begin
                (fprintf old-error-port "stdout-port ~a\n" (get-output-string stdout-port))
                (fprintf old-error-port "stderr-port ~a\n" (get-output-string stderr-port))
                (error "Failed to execute egglog"))))

        (delete-file egglog-file-path)

        (cons (get-output-string stdout-port) (get-output-string stderr-port)))))
