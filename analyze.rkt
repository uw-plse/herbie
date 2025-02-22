#lang racket

(require
        "./src/syntax/load-plugin.rkt"
        "./src/api/sandbox.rkt"
        "./src/syntax/platform.rkt"
        "src/syntax/read.rkt"
         "src/syntax/syntax.rkt"
         "src/syntax/sugar.rkt"
         "src/syntax/types.rkt"
         "src/core/points.rkt"
         "src/core/programs.rkt"
)

(load-herbie-builtins)

(define (extract-vars expr)
  (remove-duplicates
   (let loop ([expr expr])
     (match expr
       [(? symbol? var) (list var)] ; Variable
       [(list _ args ...) (append-map loop args)] ; Compound expression
       [_ '()]))))

;;; (define prog '(-.f32 x y))

;;; (displayln batch-errs)

(define (get-float-type expr)
  (cond
    [(and (pair? expr) (symbol? (car expr)) (regexp-match? #rx".f64$" (symbol->string (car expr))))
     'binary64]

    [(and (pair? expr) (symbol? (car expr)) (regexp-match? #rx".f32$" (symbol->string (car expr))))
     'binary32]

    [(pair? expr)
     (let ([subtypes (map get-float-type (cdr expr))])
       (if (member 'binary64 subtypes) 'binary64 'binary32))]

    [else 'binary32]))

  (define cost-proc
        (platform-cost-proc (*active-platform*)))


(define (process-file file-path)
  (with-input-from-file file-path
    (lambda ()
      (for ([line (in-lines)])
      (define split-line (regexp-split #px"," line))
        (define prog  (read (open-input-string (car split-line))))
        (define count  (read (open-input-string (car (cdr split-line)))))
        (define spec (prog->spec prog))
        (define vars (extract-vars prog))
        (define precon '(TRUE))
        (define float-type (get-float-type prog))

        (define ctx
            (context
                vars                     
                (get-representation float-type) 
        (map (λ (_) (get-representation float-type)) vars)))
          (define (expr->cost expr)
          (cost-proc expr (repr-of expr ctx)))

        (define cost (expr->cost prog))

        (*context* ctx)
        (*num-points* 8000) ;; can change this 
        (define pcon (sample-pcontext vars spec precon))
        (define errs (errors prog (cdr pcon) ctx))

        ;;; (define batch-errs (batch-errors prog (cdr pcon) ctx))
;;; (set! table (append table (list (list prog spec (errors-score errs)))))      
(printf "~a , ~a , ~a , ~a , ~a , ~a\n" (errors-score errs) prog spec cost count (length vars))))))

;;;   (define table
;;;     (list '("Impl" "Spec" "Error Score")))

(define file-path "1000patchedtobeanalyzed")
(process-file file-path)
;;; (pretty-print-columns table)
