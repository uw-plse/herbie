#lang racket

(provide (struct-out egglog-program)
         make-egglog-program
         egglog-program-add!
         egglog-program-add-list!
         get-current-program
         get-actual-program)

;; Track the entire Egglog program in one go by "converting" into racket based code
(struct egglog-program ([exprs #:mutable])) ; update using set-egglog-program-exprs!

;; (List Of exprs) to store the program
(define (make-egglog-program)
  (egglog-program '()))

;; Add a single expr to the program (in REVERSE)
(define (egglog-program-add! expr program)
  (set-egglog-program-exprs! program (cons expr (egglog-program-exprs program)))
  (void))

;; Append an expr-list to the program (in REVERSE)
(define (egglog-program-add-list! expr-list program)
  (set-egglog-program-exprs! program (append (reverse expr-list) (egglog-program-exprs program)))
  (void))

;; Public method to get the program list
(define (get-current-program program)
  (egglog-program-exprs program))

;; Get program as (list of exprs) in the ACTUAL order after reversing
(define (get-actual-program program)
  (reverse (get-current-program program)))
