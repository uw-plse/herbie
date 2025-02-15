#lang racket

(require rackunit
         "egglog-herbie.rkt")

(define (test-e1->expr)
  (check-equal? (e1->expr '(Num (bigrat (from-string "3") (from-string "4")))) 3/4)

  (check-equal? (e1->expr '(Var "x")) 'x)

  (check-equal? (e1->expr '(If (Var "x")
                               (Num (bigrat (from-string "1") (from-string "2")))
                               (Num (bigrat (from-string "0") (from-string "5")))))
                '(if x 1/2 0))

  (check-equal? (e1->expr '(Add (Num (bigrat (from-string "1") (from-string "1")))
                                (Num (bigrat (from-string "2") (from-string "1")))))
                '(+ 1 2))

  (check-equal? (e1->expr '(Mul (Num (bigrat (from-string "2") (from-string "1")))
                                (Num (bigrat (from-string "3") (from-string "1")))))
                '(* 2 3)))

(define (test-e2->expr)
  (check-equal? (e2->expr '(Num (bigrat (from-string "5") (from-string "6")))) 5/6)

  (check-equal? (e2->expr '(Var "y")) 'y)

  (check-equal? (e2->expr '(IfTy (Var "y")
                                 (Num (bigrat (from-string "1") (from-string "2")))
                                 (Num (bigrat (from-string "0") (from-string "1")))))
                '(if y 1/2 0))

  (check-equal? (e2->expr '(Mulf64Ty (Num (bigrat (from-string "2") (from-string "1")))
                                     (Num (bigrat (from-string "3") (from-string "1")))))
                '(*.f64 2 3))

  (check-equal? (e2->expr '(Approx (Add (Num (bigrat (from-string "1") (from-string "1")))
                                        (Num (bigrat (from-string "2") (from-string "1"))))
                                   (Mulf64Ty (Num (bigrat (from-string "3") (from-string "1")))
                                             (Num (bigrat (from-string "1") (from-string "1"))))))
                '#s(approx (+ 1 2) (*.f64 3 1))))

;;; added
; (e2->expr '(Approx (Add (Sin (Add (Var "x") (Var "eps"))))
;                    (Mul (Num (bigrat (from-string "-1") (from-string "1")))
;                         (Cos (Var "x"))))
; '(≈ (+ (sin (+ x eps))) (* -1 (cos x))))

; (check-equal?
;   (e2->expr '(Mul (Num (bigrat (from-string "3") (from-string "2")))
;                   (Add (Num (bigrat (from-string "4") (from-string "5")))
;                        (Var "z"))))
;   '(* 3/2 (+ 4/5 z)))

; (check-equal?
;   (e2->expr '(IfTy (Var "cond")
;                    (Num (bigrat (from-string "7") (from-string "8")))
;                    (Num (bigrat (from-string "-2") (from-string "3")))))
;   '(if cond 7/8 -2/3))

; (check-equal?
;   (e2->expr '(Div (Num (bigrat (from-string "1") (from-string "2")))
;                   (Mul (Var "a") (Var "b"))))
;   '(/ 1/2 (* a b)))

(define (run-tests)
  (begin
    (populate-e->id-tables)
    (test-e1->expr)
    (test-e2->expr)
    (clear-e->id-tables)))

(run-tests)
