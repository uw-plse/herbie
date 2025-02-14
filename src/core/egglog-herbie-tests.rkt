#lang racket
(require rackunit
         racket/file
         "egglog-herbie.rkt")

;; TODO : figure how to set up the ids
(define (mock-hash key)
  (hash-ref (hash 'add '+ 'mul '*) key))

(define e1->id mock-hash)
(define e2->id mock-hash)

(define (approx x y)
  `(approx ,x ,y))

(define (egglog-num? x)
  (equal? x 'num))
(define (egglog-var? x)
  (equal? x 'var))

(define (test-e1->expr)
  (check-equal? (e1->expr '(Num (bigrat (from-string "3") (from-string "4")))) 3/4)

  (check-equal? (e1->expr '(Var "x")) 'x)

  (check-equal? (e1->expr '(If (Var "x")
                               (Num (bigrat (from-string "1") (from-string "2")))
                               (Num (bigrat (from-string "0") (from-string "5")))))
                '(if x 1/2 0))

    ; (check-equal? (e1->expr '(Add (Num (bigrat (from-string "1") (from-string "1"))) (Num (bigrat (from-string "2") (from-string "1")))))
    ;               '(+ 1 2))
  )

(define (test-e2->expr)
  (check-equal? (e2->expr '(num (bigrat (from-string "5") (from-string "6")))) 5/6)
  (check-equal? (e2->expr '(var "y")) 'y)
  (check-equal? (e2->expr '(IfTy (var "y")
                                 (num (bigrat (from-string "1") (from-string "2")))
                                 (num (bigrat (from-string "0") (from-string "1")))))
                '(if y 1/2 0))
  (check-equal? (e2->expr '(mul (num (bigrat (from-string "2") (from-string "1")))
                                (num (bigrat (from-string "3") (from-string "1")))))
                '(* 2 3))
  (check-equal? (e2->expr '(Approx (add (num (bigrat (from-string "1") (from-string "1")))
                                        (num (bigrat (from-string "2") (from-string "1"))))
                                   (mul (num (bigrat (from-string "3") (from-string "1")))
                                        (num (bigrat (from-string "1") (from-string "1"))))))
                '(approx (+ 1 2) (* 3 1))))

(define (run-tests)
  (test-e1->expr)
  ;   (test-e2->expr)
  )

(run-tests)
