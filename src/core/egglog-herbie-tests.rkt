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
                '(* 2 3))

  (check-equal?
   (e1->expr '(Add (Mul (Var "a") (Div (Num (bigrat (from-string "3") (from-string "2"))) (Var "b")))
                   (Sub (Num (bigrat (from-string "5") (from-string "6")))
                        (Mul (Var "c") (Num (bigrat (from-string "-1") (from-string "4")))))))
   '(+ (* a (/ 3/2 b)) (- 5/6 (* c -1/4))))

  (check-equal? (e1->expr '(Div (Mul (Sub (Var "x") (Var "y"))
                                     (Add (Num (bigrat (from-string "7") (from-string "5")))
                                          (Var "z")))
                                (Num (bigrat (from-string "2") (from-string "3")))))
                '(/ (* (- x y) (+ 7/5 z)) 2/3))

  (check-equal? (e1->expr '(Eq (Add (Mul (Var "p") (Num (bigrat (from-string "1") (from-string "2"))))
                                    (Div (Var "q")
                                         (Num (bigrat (from-string "3") (from-string "4")))))
                               (Num (bigrat (from-string "5") (from-string "6")))))
                '(== (+ (* p 1/2) (/ q 3/4)) 5/6))

  (check-equal? (e1->expr '(Gte (Div (Var "r") (Add (Var "s") (Var "t")))
                                (Sub (Num (bigrat (from-string "8") (from-string "9")))
                                     (Mul (Num (bigrat (from-string "1") (from-string "3")))
                                          (Var "u")))))
                '(>= (/ r (+ s t)) (- 8/9 (* 1/3 u)))))

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
                '#s(approx (+ 1 2) (*.f64 3 1)))

  ;; Complex 1
  (check-equal?
   (e2->expr
    '(Approx
      (Add (Sin (Add (Var "x") (Var "eps")))
           (Mul (Num (bigrat (from-string "-1") (from-string "1"))) (Sin (Var "x"))))
      (Fmaf64Ty
       (Fmaf64Ty (Sinf64Ty (Varbinary64 "x"))
                 (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                     (Numbinary64 (bigrat (from-string "1") (from-string "24"))))
                           (Varbinary64 "eps")
                           (Numbinary64 (bigrat (from-string "-1") (from-string "2"))))
                 (Mulf64Ty (Mulf64Ty (Varbinary64 "eps")
                                     (Numbinary64 (bigrat (from-string "-1") (from-string "6"))))
                           (Cosf64Ty (Varbinary64 "x"))))
       (Mulf64Ty (Varbinary64 "eps") (Varbinary64 "eps"))
       (Mulf64Ty (Cosf64Ty (Varbinary64 "x")) (Varbinary64 "eps")))))
   '#s(approx (+ (sin (+ x eps)) (* -1 (sin x)))
              (fma.f64 (fma.f64 (sin.f64 x)
                                (fma.f64 (*.f64 eps 1/24) eps -1/2)
                                (*.f64 (*.f64 eps -1/6) (cos.f64 x)))
                       (*.f64 eps eps)
                       (*.f64 (cos.f64 x) eps))))

  ;; Complex 2
  (check-equal?
   (e2->expr
    '(Mulf64Ty
      (Fmaf64Ty
       (Mulf64Ty (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                     (Numbinary64 (bigrat (from-string "1") (from-string "24"))))
                           (Varbinary64 "eps")
                           (Numbinary64 (bigrat (from-string "-1") (from-string "2"))))
                 (Sinf64Ty (Varbinary64 "x")))
       (Varbinary64 "eps")
       (Mulf64Ty (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                     (Numbinary64 (bigrat (from-string "-1") (from-string "6"))))
                           (Varbinary64 "eps")
                           (Numbinary64 (bigrat (from-string "1") (from-string "1"))))
                 (Cosf64Ty (Varbinary64 "x"))))
      (Varbinary64 "eps")))
   '(*.f64 (fma.f64 (*.f64 (fma.f64 (*.f64 eps 1/24) eps -1/2) (sin.f64 x))
                    eps
                    (*.f64 (fma.f64 (*.f64 eps -1/6) eps 1) (cos.f64 x)))
           eps))

  ;; Complex 3
  (check-equal?
   (e2->expr
    '(Fmaf64Ty
      (Mulf64Ty (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                    (Numbinary64 (bigrat (from-string "1") (from-string "24"))))
                          (Varbinary64 "eps")
                          (Numbinary64 (bigrat (from-string "-1") (from-string "2"))))
                (Sinf64Ty (Varbinary64 "x")))
      (Varbinary64 "eps")
      (Mulf64Ty (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                    (Numbinary64 (bigrat (from-string "-1") (from-string "6"))))
                          (Varbinary64 "eps")
                          (Numbinary64 (bigrat (from-string "1") (from-string "1"))))
                (Cosf64Ty (Varbinary64 "x")))))
   '(fma.f64 (*.f64 (fma.f64 (*.f64 eps 1/24) eps -1/2) (sin.f64 x))
             eps
             (*.f64 (fma.f64 (*.f64 eps -1/6) eps 1) (cos.f64 x))))

  ;; Complex 4
  (check-equal?
   (e2->expr
    '(Fmaf64Ty (Sinf64Ty (Varbinary64 "x"))
               (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                   (Numbinary64 (bigrat (from-string "1") (from-string "24"))))
                         (Varbinary64 "eps")
                         (Numbinary64 (bigrat (from-string "-1") (from-string "2"))))
               (Mulf64Ty (Mulf64Ty (Varbinary64 "eps")
                                   (Numbinary64 (bigrat (from-string "-1") (from-string "6"))))
                         (Cosf64Ty (Varbinary64 "x")))))
   '(fma.f64 (sin.f64 x) (fma.f64 (*.f64 eps 1/24) eps -1/2) (*.f64 (*.f64 eps -1/6) (cos.f64 x))))

  (check-equal? (e2->expr '(Approx (Add (Sin (Add (Var "x") (Var "eps"))))
                                   (Mulf64Ty (Num (bigrat (from-string "-1") (from-string "1")))
                                             (Cosf64Ty (Var "x")))))
                '#s(approx (+ (sin (+ x eps))) (*.f64 -1 (cos.f64 x))))

  (check-equal? (e2->expr '(Mulf32Ty (Num (bigrat (from-string "3") (from-string "2")))
                                     (Addf32Ty (Num (bigrat (from-string "4") (from-string "5")))
                                               (Var "z"))))
                '(*.f32 3/2 (+.f32 4/5 z)))

  (check-equal? (e2->expr '(IfTy (Var "cond")
                                 (Num (bigrat (from-string "7") (from-string "8")))
                                 (Num (bigrat (from-string "-2") (from-string "3")))))
                '(if cond 7/8 -2/3)))

(define (run-tests)
  (begin
    (printf "Testing....\n")
    (populate-e->id-tables)
    (test-e1->expr)
    (test-e2->expr)
    (printf "Done.\n")))

(run-tests)
