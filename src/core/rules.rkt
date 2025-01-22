#lang racket

;; Arithmetic identities for rewriting programs.

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "programs.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt")

(provide *rules*
         *simplify-rules*
         (struct-out rule))

;; A rule represents "find-and-replacing" `input` by `output`. Both
;; are patterns, meaning that symbols represent pattern variables.
(struct rule (name input output itypes otype tags)
  #:methods gen:custom-write
  [(define (write-proc rule port mode)
     (fprintf port "#<rule ~a>" (rule-name rule)))])

(define *all-rules* '())

(define (rule-enabled? rule)
  (ormap (curry flag-set? 'rules) (rule-tags rule)))

(define ((has-tag? tag) rule)
  (set-member? (rule-tags rule) tag))

(define (*rules*)
  (filter (conjoin rule-enabled? (has-tag? 'sound)) *all-rules*))

(define (*simplify-rules*)
  (filter (conjoin rule-enabled? (has-tag? 'simplify) (has-tag? 'sound)) *all-rules*))

;;
;;  Rule loading
;;

(define (type-of-rule input output ctx)
  (let loop ([input input]
             [output output])
    (match* (input output)
      ; first, try the input expression
      ; special case for `if` expressions
      [((list 'if _ ift _) _) (loop ift output)]
      [((list op _ ...) _) (operator-info op 'otype)]
      [(_ (list 'if _ ift _)) (loop input ift)]
      [(_ (list op _ ...)) (operator-info op 'otype)]
      [((? symbol?) _) (dict-ref ctx input)]
      [(_ (? symbol?)) (dict-ref ctx output)]
      [(_ _) (error 'type-of-rule "could not compute type of rule ~a => ~a" input output)])))

(define-syntax define-ruleset*
  (syntax-rules ()
    [(define-ruleset* name groups [rname input output] ...)
     (define-ruleset* name groups #:type () [rname input output] ...)]
    [(define-ruleset* name groups #:type ([var type] ...) [rname input output] ...)
     (set! *all-rules*
           (let ([var-ctx '((var . type) ...)])
             (list* (let ([otype (type-of-rule 'input 'output var-ctx)])
                      (rule 'rname 'input 'output var-ctx otype 'groups)) ...
                    *all-rules*)))]))

; Commutativity
(define-ruleset* commutativity
                 (arithmetic simplify fp-safe sound)
                 #:type ([a real] [b real])
                 [+-commutative (+ a b) (+ b a)]
                 [*-commutative (* a b) (* b a)])

; Associativity
(define-ruleset* associativity
                 (arithmetic simplify sound)
                 #:type ([a real] [b real] [c real])
                 [associate-+r+ (+ a (+ b c)) (+ (+ a b) c)]
                 [associate-+l+ (+ (+ a b) c) (+ a (+ b c))]
                 [associate-+r- (+ a (- b c)) (- (+ a b) c)]
                 [associate-+l- (+ (- a b) c) (- a (- b c))]
                 [associate--r+ (- a (+ b c)) (- (- a b) c)]
                 [associate--l+ (- (+ a b) c) (+ a (- b c))]
                 [associate--l- (- (- a b) c) (- a (+ b c))]
                 [associate--r- (- a (- b c)) (+ (- a b) c)]
                 [associate-*r* (* a (* b c)) (* (* a b) c)]
                 [associate-*l* (* (* a b) c) (* a (* b c))]
                 [associate-*r/ (* a (/ b c)) (/ (* a b) c)]
                 [associate-*l/ (* (/ a b) c) (/ (* a c) b)]
                 [associate-/r* (/ a (* b c)) (/ (/ a b) c)]
                 [associate-/r/ (/ a (/ b c)) (* (/ a b) c)]
                 [associate-/l/ (/ (/ b c) a) (/ b (* c a))]
                 [associate-/l* (/ (* b c) a) (* b (/ c a))])

; Identity
(define-ruleset* id-reduce
                 (arithmetic simplify sound)
                 #:type ([a real])
                 [remove-double-div (/ 1 (/ 1 a)) a]
                 [rgt-mult-inverse (* a (/ 1 a)) 1]
                 [lft-mult-inverse (* (/ 1 a) a) 1]
                 [+-inverses (- a a) 0]
                 [div0 (/ 0 a) 0]
                 [mul0-lft (* 0 a) 0]
                 [mul0-rgt (* a 0) 0]
                 [*-inverses (/ a a) 1]
                 [+-lft-identity (+ 0 a) a]
                 [+-rgt-identity (+ a 0) a]
                 [--rgt-identity (- a 0) a]
                 [sub0-neg (- 0 a) (neg a)]
                 [remove-double-neg (neg (neg a)) a]
                 [*-lft-identity (* 1 a) a]
                 [*-rgt-identity (* a 1) a]
                 [/-rgt-identity (/ a 1) a]
                 [mul-1-neg (* -1 a) (neg a)])
; Counting
(define-ruleset* counting (arithmetic simplify sound) #:type ([x real]) [count-2 (+ x x) (* 2 x)])

(define-ruleset* counting-rev
                 (arithmetic simplify sound)
                 #:type ([x real])
                 [2-split 2 (+ 1 1)]
                 [count-2-rev (* 2 x) (+ x x)])
; Distributivity
(define-ruleset* distributivity
                 (arithmetic simplify sound)
                 #:type ([a real] [b real] [c real])
                 [distribute-lft-in (* a (+ b c)) (+ (* a b) (* a c))]
                 [distribute-rgt-in (* a (+ b c)) (+ (* b a) (* c a))]
                 [distribute-lft-out (+ (* a b) (* a c)) (* a (+ b c))]
                 [distribute-lft-out-- (- (* a b) (* a c)) (* a (- b c))]
                 [distribute-rgt-out (+ (* b a) (* c a)) (* a (+ b c))]
                 [distribute-rgt-out-- (- (* b a) (* c a)) (* a (- b c))]
                 [distribute-lft1-in (+ (* b a) a) (* (+ b 1) a)]
                 [distribute-rgt1-in (+ a (* c a)) (* (+ c 1) a)])
(define-ruleset* cancel-sign
                 (arithmetic simplify sound)
                 #:type ([a real] [b real] [c real])
                 [cancel-sign-sub (- a (* (neg b) c)) (+ a (* b c))]
                 [cancel-sign-sub-inv (- a (* b c)) (+ a (* (neg b) c))])
; Safe Distributiviity
(define-ruleset* distributivity-fp-safe
                 (arithmetic simplify fp-safe sound)
                 #:type ([a real] [b real])
                 [distribute-lft-neg-in (neg (* a b)) (* (neg a) b)]
                 [distribute-rgt-neg-in (neg (* a b)) (* a (neg b))]
                 [distribute-lft-neg-out (* (neg a) b) (neg (* a b))]
                 [distribute-rgt-neg-out (* a (neg b)) (neg (* a b))]
                 [distribute-neg-in (neg (+ a b)) (+ (neg a) (neg b))]
                 [distribute-neg-out (+ (neg a) (neg b)) (neg (+ a b))]
                 [distribute-frac-neg (/ (neg a) b) (neg (/ a b))]
                 [distribute-frac-neg2 (/ a (neg b)) (neg (/ a b))]
                 [distribute-neg-frac (neg (/ a b)) (/ (neg a) b)]
                 [distribute-neg-frac2 (neg (/ a b)) (/ a (neg b))])

(define-ruleset* cancel-sign-fp-safe
                 (arithmetic simplify fp-safe sound)
                 #:type ([a real] [b real] [c real])
                 [fp-cancel-sign-sub (- a (* (neg b) c)) (+ a (* b c))]
                 [fp-cancel-sub-sign (+ a (* (neg b) c)) (- a (* b c))])

(define-ruleset* cancel-sign-fp-safe-rev
                 (arithmetic simplify fp-safe sound)
                 #:type ([a real] [b real] [c real])
                 [fp-cancel-sign-sub-inv (+ a (* b c)) (- a (* (neg b) c))]
                 [fp-cancel-sub-sign-inv (- a (* b c)) (+ a (* (neg b) c))])

; Difference of squares
(define-ruleset* difference-of-squares-canonicalize
                 (polynomials simplify sound)
                 #:type ([a real] [b real])
                 [swap-sqr (* (* a b) (* a b)) (* (* a a) (* b b))]
                 [unswap-sqr (* (* a a) (* b b)) (* (* a b) (* a b))]
                 [difference-of-squares (- (* a a) (* b b)) (* (+ a b) (- a b))]
                 [difference-of-sqr-1 (- (* a a) 1) (* (+ a 1) (- a 1))]
                 [difference-of-sqr--1 (+ (* a a) -1) (* (+ a 1) (- a 1))]
                 [pow-sqr (* (pow a b) (pow a b)) (pow a (* 2 b))])

(define-ruleset* difference-of-squares-canonicalize-rev
                 (polynomials simplify sound)
                 #:type ([a real] [b real])
                 [difference-of-sqr-1-rev (* (+ a 1) (- a 1)) (- (* a a) 1)]
                 [difference-of-sqr--1-rev (* (+ a 1) (- a 1)) (+ (* a a) -1)]
                 [difference-of-squares-rev (* (+ a b) (- a b)) (- (* a a) (* b b))])
(define-ruleset* sqr-pow-expand
                 (polynomials)
                 #:type ([a real] [b real])
                 [sqr-pow (pow a b) (* (pow a (/ b 2)) (pow a (/ b 2)))])

(define-ruleset* difference-of-squares-flip
                 (polynomials)
                 #:type ([a real] [b real])
                 [flip-+ (+ a b) (/ (- (* a a) (* b b)) (- a b))]
                 [flip-- (- a b) (/ (- (* a a) (* b b)) (+ a b))])

; Difference of cubes
(define-ruleset*
 difference-of-cubes
 (polynomials sound)
 #:type ([a real] [b real])
 [sum-cubes (+ (pow a 3) (pow b 3)) (* (+ (* a a) (- (* b b) (* a b))) (+ a b))]
 [difference-cubes (- (pow a 3) (pow b 3)) (* (+ (* a a) (+ (* b b) (* a b))) (- a b))]
 [flip3-+ (+ a b) (/ (+ (pow a 3) (pow b 3)) (+ (* a a) (- (* b b) (* a b))))]
 [flip3-- (- a b) (/ (- (pow a 3) (pow b 3)) (+ (* a a) (+ (* b b) (* a b))))])

(define-ruleset*
 difference-of-cubes-rev
 (polynomials sound)
 #:type ([a real] [b real])
 [difference-cubes-rev (* (+ (* a a) (+ (* b b) (* a b))) (- a b)) (- (pow a 3) (pow b 3))]
 [sum-cubes-rev (* (+ (* a a) (- (* b b) (* a b))) (+ a b)) (+ (pow a 3) (pow b 3))])

; Dealing with fractions
(define-ruleset* fractions-distribute
                 (fractions simplify sound)
                 #:type ([a real] [b real] [c real] [d real])
                 [div-sub (/ (- a b) c) (- (/ a c) (/ b c))]
                 [times-frac (/ (* a b) (* c d)) (* (/ a c) (/ b d))]
                 [div-add (/ (+ a b) c) (+ (/ a c) (/ b c))])

(define-ruleset* fractions-distribute-rev
                 (fractions simplify sound)
                 #:type ([a real] [b real] [c real] [d real])
                 [div-add-rev (+ (/ a c) (/ b c)) (/ (+ a b) c)])

(define-ruleset* fractions-transform
                 (fractions sound)
                 #:type ([a real] [b real] [c real] [d real])
                 [sub-div (- (/ a c) (/ b c)) (/ (- a b) c)]
                 [frac-add (+ (/ a b) (/ c d)) (/ (+ (* a d) (* b c)) (* b d))]
                 [frac-sub (- (/ a b) (/ c d)) (/ (- (* a d) (* b c)) (* b d))]
                 [frac-times (* (/ a b) (/ c d)) (/ (* a c) (* b d))]
                 [frac-2neg (/ a b) (/ (neg a) (neg b))])

(define-ruleset* fractions-transform-rev
                 (fractions sound)
                 #:type ([a real] [b real] [c real] [d real])
                 [frac-2neg-rev (/ (neg a) (neg b)) (/ a b)])

; Square root
(define-ruleset* squares-reduce
                 (arithmetic simplify sound)
                 #:type ([x real])
                 [rem-square-sqrt (* (sqrt x) (sqrt x)) x]
                 [rem-sqrt-square (sqrt (* x x)) (fabs x)]
                 [rem-sqrt-square-rev (fabs x) (sqrt (* x x))])

(define-ruleset* squares-reduce-fp-sound
                 (arithmetic simplify fp-safe sound)
                 #:type ([x real])
                 [sqr-neg (* (neg x) (neg x)) (* x x)]
                 [sqr-abs (* (fabs x) (fabs x)) (* x x)])

(define-ruleset* squares-reduce-fp-sound-rev
                 (arithmetic simplify fp-safe sound)
                 #:type ([x real])
                 [sqr-abs-rev (* x x) (* (fabs x) (fabs x))]
                 [sqr-neg-rev (* x x) (* (neg x) (neg x))])

(define-ruleset* fabs-reduce
                 (arithmetic simplify fp-safe sound)
                 #:type ([x real] [a real] [b real])
                 [fabs-fabs (fabs (fabs x)) (fabs x)]
                 [fabs-sub (fabs (- a b)) (fabs (- b a))]
                 [fabs-neg (fabs (neg x)) (fabs x)]
                 [fabs-sqr (fabs (* x x)) (* x x)]
                 [fabs-mul (fabs (* a b)) (* (fabs a) (fabs b))]
                 [fabs-div (fabs (/ a b)) (/ (fabs a) (fabs b))])

(define-ruleset* fabs-expand
                 (arithmetic fp-safe sound)
                 #:type ([x real] [a real] [b real])
                 [neg-fabs (fabs x) (fabs (neg x))]
                 [mul-fabs (* (fabs a) (fabs b)) (fabs (* a b))]
                 [div-fabs (/ (fabs a) (fabs b)) (fabs (/ a b))])

(define-ruleset* squares-transform-sound
                 (arithmetic sound)
                 #:type ([x real] [y real])
                 [sqrt-pow2 (pow (sqrt x) y) (pow x (/ y 2))]
                 [sqrt-unprod (* (sqrt x) (sqrt y)) (sqrt (* x y))]
                 [sqrt-undiv (/ (sqrt x) (sqrt y)) (sqrt (/ x y))])

(define-ruleset* squares-transform
                 (arithmetic)
                 #:type ([x real] [y real])
                 [sqrt-pow1 (sqrt (pow x y)) (pow x (/ y 2))]
                 [sqrt-prod (sqrt (* x y)) (* (sqrt x) (sqrt y))]
                 [sqrt-div (sqrt (/ x y)) (/ (sqrt x) (sqrt y))]
                 [add-sqr-sqrt x (* (sqrt x) (sqrt x))])

; Cube root
(define-ruleset* cubes-reduce
                 (arithmetic simplify sound)
                 #:type ([x real])
                 [rem-cube-cbrt (pow (cbrt x) 3) x]
                 [rem-cbrt-cube (cbrt (pow x 3)) x]
                 [rem-3cbrt-lft (* (* (cbrt x) (cbrt x)) (cbrt x)) x]
                 [rem-3cbrt-rft (* (cbrt x) (* (cbrt x) (cbrt x))) x]
                 [cube-neg (pow (neg x) 3) (neg (pow x 3))]
                 [cube-neg-rev (neg (pow x 3)) (pow (neg x) 3)])

(define-ruleset* cubes-distribute
                 (arithmetic simplify sound)
                 #:type ([x real] [y real])
                 [cube-prod (pow (* x y) 3) (* (pow x 3) (pow y 3))]
                 [cube-div (pow (/ x y) 3) (/ (pow x 3) (pow y 3))]
                 [cube-mult (pow x 3) (* x (* x x))]
                 [cube-prod-rev (* (pow x 3) (pow y 3)) (pow (* x y) 3)]
                 [cube-div-rev (/ (pow x 3) (pow y 3)) (pow (/ x y) 3)])

(define-ruleset* cubes-transform
                 (arithmetic sound)
                 #:type ([x real] [y real])
                 [cbrt-prod (cbrt (* x y)) (* (cbrt x) (cbrt y))]
                 [cbrt-div (cbrt (/ x y)) (/ (cbrt x) (cbrt y))]
                 [cbrt-unprod (* (cbrt x) (cbrt y)) (cbrt (* x y))]
                 [cbrt-undiv (/ (cbrt x) (cbrt y)) (cbrt (/ x y))]
                 [add-cube-cbrt x (* (* (cbrt x) (cbrt x)) (cbrt x))]
                 [add-cbrt-cube x (cbrt (* (* x x) x))])

(define-ruleset* cubes-canonicalize
                 (arithmetic simplify sound)
                 #:type ([x real])
                 [cube-unmult (* x (* x x)) (pow x 3)])

(define-ruleset* exp-expand-sound (exponents sound) #:type ([x real]) [add-log-exp x (log (exp x))])

; Exponentials
(define-ruleset* exp-expand (exponents) #:type ([x real]) [add-exp-log x (exp (log x))])

(define-ruleset* exp-reduce
                 (exponents simplify sound)
                 #:type ([x real])
                 [rem-exp-log (exp (log x)) x]
                 [rem-log-exp (log (exp x)) x])

(define-ruleset* exp-constants
                 (exponents simplify fp-safe sound)
                 [exp-0 (exp 0) 1]
                 [exp-1-e (exp 1) (E)]
                 [1-exp 1 (exp 0)]
                 [e-exp-1 (E) (exp 1)])

(define-ruleset* exp-distribute
                 (exponents simplify sound)
                 #:type ([a real] [b real])
                 [exp-sum (exp (+ a b)) (* (exp a) (exp b))]
                 [exp-neg (exp (neg a)) (/ 1 (exp a))]
                 [exp-diff (exp (- a b)) (/ (exp a) (exp b))])

(define-ruleset* exp-factor
                 (exponents simplify sound)
                 #:type ([a real] [b real])
                 [prod-exp (* (exp a) (exp b)) (exp (+ a b))]
                 [rec-exp (/ 1 (exp a)) (exp (neg a))]
                 [div-exp (/ (exp a) (exp b)) (exp (- a b))]
                 [exp-prod (exp (* a b)) (pow (exp a) b)]
                 [exp-sqrt (exp (/ a 2)) (sqrt (exp a))]
                 [exp-cbrt (exp (/ a 3)) (cbrt (exp a))]
                 [exp-lft-sqr (exp (* a 2)) (* (exp a) (exp a))]
                 [exp-lft-cube (exp (* a 3)) (pow (exp a) 3)])
(define-ruleset* exp-factor-rev
                 (exponents simplify sound)
                 #:type ([a real] [b real])
                 [exp-cbrt-rev (cbrt (exp a)) (exp (/ a 3))]
                 [exp-lft-cube-rev (pow (exp a) 3) (exp (* a 3))]
                 [exp-sqrt-rev (sqrt (exp a)) (exp (/ a 2))]
                 [exp-lft-sqr-rev (* (exp a) (exp a)) (exp (* a 2))])
; Powers
(define-ruleset* pow-reduce (exponents simplify sound) #:type ([a real]) [unpow-1 (pow a -1) (/ 1 a)])

(define-ruleset* pow-reduce-fp-safe
                 (exponents simplify fp-safe sound)
                 #:type ([a real])
                 [unpow1 (pow a 1) a])

(define-ruleset* pow-reduce-fp-safe-nan
                 (exponents simplify fp-safe-nan sound)
                 #:type ([a real])
                 [unpow0 (pow a 0) 1]
                 [pow-base-1 (pow 1 a) 1])

(define-ruleset* pow-expand-fp-safe (exponents fp-safe sound) #:type ([a real]) [pow1 a (pow a 1)])

(define-ruleset* pow-canonicalize
                 (exponents simplify sound)
                 #:type ([a real] [b real])
                 [exp-to-pow (exp (* (log a) b)) (pow a b)]
                 [unpow1/2 (pow a 1/2) (sqrt a)]
                 [unpow2 (pow a 2) (* a a)]
                 [unpow3 (pow a 3) (* (* a a) a)]
                 [unpow1/3 (pow a 1/3) (cbrt a)]
                 [pow-plus (* (pow a b) a) (pow a (+ b 1))])

(define-ruleset* pow-canonicalize-rev
                 (exponents simplify sound)
                 #:type ([a real] [b real])
                 [pow-plus-rev (pow a (+ b 1)) (* (pow a b) a)])

(define-ruleset* pow-transform-sound
                 (exponents sound)
                 #:type ([a real] [b real] [c real])
                 [pow-exp (pow (exp a) b) (exp (* a b))]
                 [pow-prod-down (* (pow b a) (pow c a)) (pow (* b c) a)]
                 [pow-prod-up (* (pow a b) (pow a c)) (pow a (+ b c))]
                 [pow-flip (/ 1 (pow a b)) (pow a (neg b))]
                 [pow-neg (pow a (neg b)) (/ 1 (pow a b))]
                 [pow-div (/ (pow a b) (pow a c)) (pow a (- b c))])

(define-ruleset* pow-specialize-sound
                 (exponents sound)
                 #:type ([a real])
                 [pow1/2 (sqrt a) (pow a 1/2)]
                 [pow2 (* a a) (pow a 2)]
                 [pow1/3 (cbrt a) (pow a 1/3)]
                 [pow3 (* (* a a) a) (pow a 3)])

(define-ruleset* pow-transform
                 (exponents)
                 #:type ([a real] [b real] [c real])
                 [pow-to-exp (pow a b) (exp (* (log a) b))]
                 [pow-sub (pow a (- b c)) (/ (pow a b) (pow a c))]
                 [pow-pow (pow (pow a b) c) (pow a (* b c))]
                 [pow-unpow (pow a (* b c)) (pow (pow a b) c)]
                 [unpow-prod-up (pow a (+ b c)) (* (pow a b) (pow a c))]
                 [unpow-prod-down (pow (* b c) a) (* (pow b a) (pow c a))])

(define-ruleset* pow-transform-fp-safe-nan
                 (exponents simplify fp-safe-nan sound)
                 #:type ([a real])
                 [pow-base-0 (pow 0 a) 0])

(define-ruleset* pow-transform-fp-safe
                 (exponents sound)
                 #:type ([a real])
                 [inv-pow (/ 1 a) (pow a -1)])

(define-ruleset* log-distribute-sound
                 (exponents simplify sound)
                 #:type ([a real] [b real])
                 [log-rec (log (/ 1 a)) (neg (log a))]
                 [log-E (log (E)) 1])

; Logarithms
(define-ruleset* log-distribute
                 (exponents)
                 #:type ([a real] [b real])
                 [log-prod (log (* a b)) (+ (log a) (log b))]
                 [log-div (log (/ a b)) (- (log a) (log b))]
                 [log-pow (log (pow a b)) (* b (log a))])

(define-ruleset* log-distribute-rev
                 (exponents)
                 #:type ([a real] [b real])
                 [log-pow-rev (* b (log a)) (log (pow a b))])

(define-ruleset* log-factor
                 (exponents sound)
                 #:type ([a real] [b real])
                 [sum-log (+ (log a) (log b)) (log (* a b))]
                 [diff-log (- (log a) (log b)) (log (/ a b))]
                 [neg-log (neg (log a)) (log (/ 1 a))])

; Trigonometry
(define-ruleset* trig-reduce-fp-sound
                 (trigonometry simplify fp-safe sound)
                 [sin-0 (sin 0) 0]
                 [cos-0 (cos 0) 1]
                 [tan-0 (tan 0) 0])

(define-ruleset* trig-reduce-fp-sound-nan
                 (trigonometry simplify fp-safe-nan sound)
                 #:type ([x real])
                 [sin-neg (sin (neg x)) (neg (sin x))]
                 [cos-neg (cos (neg x)) (cos x)]
                 [tan-neg (tan (neg x)) (neg (tan x))])

(define-ruleset* trig-reduce-fp-sound-nan-rev
                 (trigonometry simplify fp-safe-nan sound)
                 #:type ([x real])
                 [cos-neg-rev (cos x) (cos (neg x))]
                 [sin-neg-rev (neg (sin x)) (sin (neg x))]
                 [tan-neg-rev (neg (tan x)) (tan (neg x))])

(define-ruleset* trig-expand-fp-safe
                 (trignometry fp-safe sound)
                 #:type ([x real])
                 [sqr-sin-b (* (sin x) (sin x)) (- 1 (* (cos x) (cos x)))]
                 [sqr-cos-b (* (cos x) (cos x)) (- 1 (* (sin x) (sin x)))]
                 [sqr-cos-b-rev (- 1 (* (sin x) (sin x))) (* (cos x) (cos x))]
                 [sqr-sin-b-rev (- 1 (* (cos x) (cos x))) (* (sin x) (sin x))])

(define-ruleset*
 trig-inverses
 (trigonometry sound)
 #:type ([x real])
 [sin-asin (sin (asin x)) x]
 [cos-acos (cos (acos x)) x]
 [tan-atan (tan (atan x)) x]
 [atan-tan (atan (tan x)) (remainder x (PI))]
 [asin-sin (asin (sin x)) (- (fabs (remainder (+ x (/ (PI) 2)) (* 2 (PI)))) (/ (PI) 2))]
 [acos-cos (acos (cos x)) (fabs (remainder x (* 2 (PI))))])

(define-ruleset*
 trig-inverses-rev
 (trigonometry sound)
 #:type ([x real])
 [acos-cos-rev (fabs (remainder x (* 2 (PI)))) (acos (cos x))]
 [asin-sin-rev (- (fabs (remainder (+ x (/ (PI) 2)) (* 2 (PI)))) (/ (PI) 2)) (asin (sin x))]
 [atan-tan-rev (remainder x (PI)) (atan (tan x))])

(define-ruleset* trig-inverses-simplified
                 (trigonometry)
                 #:type ([x real])
                 [atan-tan-s (atan (tan x)) x]
                 [asin-sin-s (asin (sin x)) x]
                 [acos-cos-s (acos (cos x)) x])

(define-ruleset* trig-reduce-expressions
                 (trigonometry simplify sound)
                 #:type ([a real] [b real] [x real])
                 [cos-sin-sum (+ (* (cos a) (cos a)) (* (sin a) (sin a))) 1]
                 [1-sub-cos (- 1 (* (cos a) (cos a))) (* (sin a) (sin a))]
                 [1-sub-sin (- 1 (* (sin a) (sin a))) (* (cos a) (cos a))]
                 [-1-add-cos (+ (* (cos a) (cos a)) -1) (neg (* (sin a) (sin a)))]
                 [-1-add-sin (+ (* (sin a) (sin a)) -1) (neg (* (cos a) (cos a)))]
                 [sub-1-cos (- (* (cos a) (cos a)) 1) (neg (* (sin a) (sin a)))]
                 [sub-1-sin (- (* (sin a) (sin a)) 1) (neg (* (cos a) (cos a)))]
                 [sin-PI/6 (sin (/ (PI) 6)) 1/2]
                 [sin-PI/4 (sin (/ (PI) 4)) (/ (sqrt 2) 2)]
                 [sin-PI/3 (sin (/ (PI) 3)) (/ (sqrt 3) 2)]
                 [sin-PI/2 (sin (/ (PI) 2)) 1]
                 [sin-PI (sin (PI)) 0]
                 [sin-+PI (sin (+ x (PI))) (neg (sin x))]
                 [sin-+PI/2 (sin (+ x (/ (PI) 2))) (cos x)]
                 [cos-PI/6 (cos (/ (PI) 6)) (/ (sqrt 3) 2)]
                 [cos-PI/4 (cos (/ (PI) 4)) (/ (sqrt 2) 2)]
                 [cos-PI/3 (cos (/ (PI) 3)) 1/2]
                 [cos-PI/2 (cos (/ (PI) 2)) 0]
                 [cos-PI (cos (PI)) -1]
                 [cos-+PI (cos (+ x (PI))) (neg (cos x))]
                 [cos-+PI/2 (cos (+ x (/ (PI) 2))) (neg (sin x))]
                 [tan-PI/6 (tan (/ (PI) 6)) (/ 1 (sqrt 3))]
                 [tan-PI/4 (tan (/ (PI) 4)) 1]
                 [tan-PI/3 (tan (/ (PI) 3)) (sqrt 3)]
                 [tan-PI (tan (PI)) 0]
                 [tan-+PI (tan (+ x (PI))) (tan x)]
                 [hang-0p-tan (/ (sin a) (+ 1 (cos a))) (tan (/ a 2))]
                 [hang-0m-tan (/ (neg (sin a)) (+ 1 (cos a))) (tan (/ (neg a) 2))]
                 [hang-p0-tan (/ (- 1 (cos a)) (sin a)) (tan (/ a 2))]
                 [hang-m0-tan (/ (- 1 (cos a)) (neg (sin a))) (tan (/ (neg a) 2))]
                 [hang-p-tan (/ (+ (sin a) (sin b)) (+ (cos a) (cos b))) (tan (/ (+ a b) 2))]
                 [hang-m-tan (/ (- (sin a) (sin b)) (+ (cos a) (cos b))) (tan (/ (- a b) 2))])

(define-ruleset* trig-reduce-expressions-rev
                 (trigonometry simplify sound)
                 #:type ([a real] [b real] [x real])
                 [1-sub-sin-rev (* (cos a) (cos a)) (- 1 (* (sin a) (sin a)))]
                 [hang-m0-tan-rev (tan (/ (neg a) 2)) (/ (- 1 (cos a)) (neg (sin a)))]
                 [hang-p0-tan-rev (tan (/ a 2)) (/ (- 1 (cos a)) (sin a))]
                 [hang-0m-tan-rev (tan (/ (neg a) 2)) (/ (neg (sin a)) (+ 1 (cos a)))]
                 [hang-0p-tan-rev (tan (/ a 2)) (/ (sin a) (+ 1 (cos a)))]
                 [tan-+PI-rev (tan x) (tan (+ x (PI)))]
                 [cos-+PI/2-rev (neg (sin x)) (cos (+ x (/ (PI) 2)))]
                 [sin-+PI/2-rev (cos x) (sin (+ x (/ (PI) 2)))]
                 [sin-+PI-rev (neg (sin x)) (sin (+ x (PI)))]
                 [cos-+PI-rev (neg (cos x)) (cos (+ x (PI)))])

(define-ruleset* trig-reduce
                 (trigonometry)
                 #:type ([a real] [b real] [x real])
                 [neg-tan-+PI/2 (tan (+ x (/ (PI) 2))) (/ -1 (tan x))]
                 [tan-+PI/2 (tan (+ (neg x) (/ (PI) 2))) (/ 1 (tan x))])

(define-ruleset* trig-reduce-rev
                 (trigonometry)
                 #:type ([a real] [b real] [x real])
                 [neg-tan-+PI/2-rev (/ -1 (tan x)) (tan (+ x (/ (PI) 2)))]
                 [tan-+PI/2-rev (/ 1 (tan x)) (tan (+ (neg x) (/ (PI) 2)))])

(define-ruleset* trig-expand-sound
                 (trigonometry sound)
                 #:type ([x real] [y real] [a real] [b real])
                 [sin-sum (sin (+ x y)) (+ (* (sin x) (cos y)) (* (cos x) (sin y)))]
                 [cos-sum (cos (+ x y)) (- (* (cos x) (cos y)) (* (sin x) (sin y)))]
                 [tan-sum (tan (+ x y)) (/ (+ (tan x) (tan y)) (- 1 (* (tan x) (tan y))))]
                 [sin-diff (sin (- x y)) (- (* (sin x) (cos y)) (* (cos x) (sin y)))]
                 [cos-diff (cos (- x y)) (+ (* (cos x) (cos y)) (* (sin x) (sin y)))]
                 [sin-2 (sin (* 2 x)) (* 2 (* (sin x) (cos x)))]
                 [sin-3 (sin (* 3 x)) (- (* 3 (sin x)) (* 4 (pow (sin x) 3)))]
                 [2-sin (* 2 (* (sin x) (cos x))) (sin (* 2 x))]
                 [3-sin (- (* 3 (sin x)) (* 4 (pow (sin x) 3))) (sin (* 3 x))]
                 [cos-2 (cos (* 2 x)) (- (* (cos x) (cos x)) (* (sin x) (sin x)))]
                 [cos-3 (cos (* 3 x)) (- (* 4 (pow (cos x) 3)) (* 3 (cos x)))]
                 [2-cos (- (* (cos x) (cos x)) (* (sin x) (sin x))) (cos (* 2 x))]
                 [3-cos (- (* 4 (pow (cos x) 3)) (* 3 (cos x))) (cos (* 3 x))])

(define-ruleset* trig-expand-sound-rev
                 (trigonometry sound)
                 #:type ([x real] [y real] [a real] [b real])
                 [cos-diff-rev (+ (* (cos x) (cos y)) (* (sin x) (sin y))) (cos (- x y))]
                 [sin-diff-rev (- (* (sin x) (cos y)) (* (cos x) (sin y))) (sin (- x y))]
                 [sin-sum-rev (+ (* (sin x) (cos y)) (* (cos x) (sin y))) (sin (+ x y))]
                 [tan-sum-rev (/ (+ (tan x) (tan y)) (- 1 (* (tan x) (tan y)))) (tan (+ x y))]
                 [cos-sum-rev (- (* (cos x) (cos y)) (* (sin x) (sin y))) (cos (+ x y))])

(define-ruleset* trig-expand-sound2
                 (trigonometry sound)
                 #:type ([x real] [y real] [a real] [b real])
                 [sqr-sin-a (* (sin x) (sin x)) (- 1/2 (* 1/2 (cos (* 2 x))))]
                 [sqr-cos-a (* (cos x) (cos x)) (+ 1/2 (* 1/2 (cos (* 2 x))))]
                 [diff-sin (- (sin x) (sin y)) (* 2 (* (sin (/ (- x y) 2)) (cos (/ (+ x y) 2))))]
                 [diff-cos (- (cos x) (cos y)) (* -2 (* (sin (/ (- x y) 2)) (sin (/ (+ x y) 2))))]
                 [sum-sin (+ (sin x) (sin y)) (* 2 (* (sin (/ (+ x y) 2)) (cos (/ (- x y) 2))))]
                 [sum-cos (+ (cos x) (cos y)) (* 2 (* (cos (/ (+ x y) 2)) (cos (/ (- x y) 2))))]
                 [cos-mult (* (cos x) (cos y)) (/ (+ (cos (+ x y)) (cos (- x y))) 2)]
                 [sin-mult (* (sin x) (sin y)) (/ (- (cos (- x y)) (cos (+ x y))) 2)]
                 [sin-cos-mult (* (sin x) (cos y)) (/ (+ (sin (- x y)) (sin (+ x y))) 2)]
                 [diff-atan (- (atan x) (atan y)) (atan2 (- x y) (+ 1 (* x y)))]
                 [sum-atan (+ (atan x) (atan y)) (atan2 (+ x y) (- 1 (* x y)))]
                 [tan-quot (tan x) (/ (sin x) (cos x))]
                 [quot-tan (/ (sin x) (cos x)) (tan x)]
                 [tan-2 (tan (* 2 x)) (/ (* 2 (tan x)) (- 1 (* (tan x) (tan x))))]
                 [2-tan (/ (* 2 (tan x)) (- 1 (* (tan x) (tan x)))) (tan (* 2 x))])

(define-ruleset* trig-expand-sound2-rev
                 (trigonometry sound)
                 #:type ([x real] [y real])
                 [diff-cos-rev (* -2 (* (sin (/ (- x y) 2)) (sin (/ (+ x y) 2)))) (- (cos x) (cos y))]
                 [diff-sin-rev (* 2 (* (sin (/ (- x y) 2)) (cos (/ (+ x y) 2)))) (- (sin x) (sin y))]
                 [diff-atan-rev (atan2 (- x y) (+ 1 (* x y))) (- (atan x) (atan y))]
                 [sum-sin-rev (* 2 (* (sin (/ (+ x y) 2)) (cos (/ (- x y) 2)))) (+ (sin x) (sin y))]
                 [sum-cos-rev (* 2 (* (cos (/ (+ x y) 2)) (cos (/ (- x y) 2)))) (+ (cos x) (cos y))]
                 [sum-atan-rev (atan2 (+ x y) (- 1 (* x y))) (+ (atan x) (atan y))]
                 [sqr-cos-a-rev (+ 1/2 (* 1/2 (cos (* 2 x)))) (* (cos x) (cos x))]
                 [sqr-sin-a-rev (- 1/2 (* 1/2 (cos (* 2 x)))) (* (sin x) (sin x))]
                 [cos-mult-rev (/ (+ (cos (+ x y)) (cos (- x y))) 2) (* (cos x) (cos y))]
                 [sin-mult-rev (/ (- (cos (- x y)) (cos (+ x y))) 2) (* (sin x) (sin y))]
                 [sin-cos-mult-rev (/ (+ (sin (- x y)) (sin (+ x y))) 2) (* (sin x) (cos y))])

(define-ruleset* trig-expand
                 (trigonometry)
                 #:type ([x real] [y real] [a real] [b real])
                 [tan-hang-p (tan (/ (+ a b) 2)) (/ (+ (sin a) (sin b)) (+ (cos a) (cos b)))]
                 [tan-hang-m (tan (/ (- a b) 2)) (/ (- (sin a) (sin b)) (+ (cos a) (cos b)))])

(define-ruleset* atrig-expand
                 (trigonometry sound)
                 #:type ([x real])
                 [cos-asin (cos (asin x)) (sqrt (- 1 (* x x)))]
                 [tan-asin (tan (asin x)) (/ x (sqrt (- 1 (* x x))))]
                 [sin-acos (sin (acos x)) (sqrt (- 1 (* x x)))]
                 [tan-acos (tan (acos x)) (/ (sqrt (- 1 (* x x))) x)]
                 [sin-atan (sin (atan x)) (/ x (sqrt (+ 1 (* x x))))]
                 [cos-atan (cos (atan x)) (/ 1 (sqrt (+ 1 (* x x))))]
                 [asin-acos (asin x) (- (/ (PI) 2) (acos x))]
                 [acos-asin (acos x) (- (/ (PI) 2) (asin x))]
                 [asin-neg (asin (neg x)) (neg (asin x))]
                 [acos-neg (acos (neg x)) (- (PI) (acos x))]
                 [atan-neg (atan (neg x)) (neg (atan x))])

(define-ruleset* atrig-expand-rev
                 (trigonometry sound)
                 #:type ([x real])
                 [acos-asin-rev (- (/ (PI) 2) (asin x)) (acos x)]
                 [asin-acos-rev (- (/ (PI) 2) (acos x)) (asin x)]
                 [asin-neg-rev (neg (asin x)) (asin (neg x))]
                 [atan-neg-rev (neg (atan x)) (atan (neg x))]
                 [acos-neg-rev (- (PI) (acos x)) (acos (neg x))]
                 [cos-atan-rev (/ 1 (sqrt (+ 1 (* x x)))) (cos (atan x))]
                 [tan-acos-rev (/ (sqrt (- 1 (* x x))) x) (tan (acos x))]
                 [tan-asin-rev (/ x (sqrt (- 1 (* x x)))) (tan (asin x))]
                 [cos-asin-rev (sqrt (- 1 (* x x))) (cos (asin x))]
                 [sin-atan-rev (/ x (sqrt (+ 1 (* x x)))) (sin (atan x))]
                 [sin-acos-rev (sqrt (- 1 (* x x))) (sin (acos x))])

; Hyperbolic trigonometric functions
(define-ruleset* htrig-reduce
                 (hyperbolic simplify sound)
                 #:type ([x real])
                 [sinh-def (sinh x) (/ (- (exp x) (exp (neg x))) 2)]
                 [cosh-def (cosh x) (/ (+ (exp x) (exp (neg x))) 2)]
                 [tanh-def-a (tanh x) (/ (- (exp x) (exp (neg x))) (+ (exp x) (exp (neg x))))]
                 [tanh-def-b (tanh x) (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1))]
                 [tanh-def-c (tanh x) (/ (- 1 (exp (* -2 x))) (+ 1 (exp (* -2 x))))]
                 [sinh-cosh (- (* (cosh x) (cosh x)) (* (sinh x) (sinh x))) 1]
                 [sinh-+-cosh (+ (cosh x) (sinh x)) (exp x)]
                 [sinh---cosh (- (cosh x) (sinh x)) (exp (neg x))])

(define-ruleset* htrig-reduce-rev
                 (hyperbolic simplify sound)
                 #:type ([x real])
                 [tanh-def-b-rev (/ (- (exp (* 2 x)) 1) (+ (exp (* 2 x)) 1)) (tanh x)]
                 [tanh-def-c-rev (/ (- 1 (exp (* -2 x))) (+ 1 (exp (* -2 x)))) (tanh x)]
                 [sinh-def-rev (/ (- (exp x) (exp (neg x))) 2) (sinh x)]
                 [cosh-def-rev (/ (+ (exp x) (exp (neg x))) 2) (cosh x)]
                 [sinh-+-cosh-rev (exp x) (+ (cosh x) (sinh x))]
                 [sinh---cosh-rev (exp (neg x)) (- (cosh x) (sinh x))])

(define-ruleset* htrig-expand-sound
                 (hyperbolic sound)
                 #:type ([x real] [y real])
                 [sinh-undef (- (exp x) (exp (neg x))) (* 2 (sinh x))]
                 [cosh-undef (+ (exp x) (exp (neg x))) (* 2 (cosh x))]
                 [tanh-undef (/ (- (exp x) (exp (neg x))) (+ (exp x) (exp (neg x)))) (tanh x)] ;
                 [cosh-sum (cosh (+ x y)) (+ (* (cosh x) (cosh y)) (* (sinh x) (sinh y)))]
                 [cosh-diff (cosh (- x y)) (- (* (cosh x) (cosh y)) (* (sinh x) (sinh y)))]
                 [cosh-2 (cosh (* 2 x)) (+ (* (sinh x) (sinh x)) (* (cosh x) (cosh x)))]
                 [cosh-1/2 (cosh (/ x 2)) (sqrt (/ (+ (cosh x) 1) 2))]
                 [sinh-sum (sinh (+ x y)) (+ (* (sinh x) (cosh y)) (* (cosh x) (sinh y)))]
                 [sinh-diff (sinh (- x y)) (- (* (sinh x) (cosh y)) (* (cosh x) (sinh y)))]
                 [sinh-2 (sinh (* 2 x)) (* 2 (* (sinh x) (cosh x)))]
                 [sinh-1/2 (sinh (/ x 2)) (/ (sinh x) (sqrt (* 2 (+ (cosh x) 1))))]
                 [tanh-2 (tanh (* 2 x)) (/ (* 2 (tanh x)) (+ 1 (* (tanh x) (tanh x))))]
                 [tanh-1/2 (tanh (/ x 2)) (/ (sinh x) (+ (cosh x) 1))]
                 [sum-sinh (+ (sinh x) (sinh y)) (* 2 (* (sinh (/ (+ x y) 2)) (cosh (/ (- x y) 2))))]
                 [sum-cosh (+ (cosh x) (cosh y)) (* 2 (* (cosh (/ (+ x y) 2)) (cosh (/ (- x y) 2))))]
                 [diff-sinh (- (sinh x) (sinh y)) (* 2 (* (cosh (/ (+ x y) 2)) (sinh (/ (- x y) 2))))]
                 [diff-cosh (- (cosh x) (cosh y)) (* 2 (* (sinh (/ (+ x y) 2)) (sinh (/ (- x y) 2))))]
                 [tanh-sum (tanh (+ x y)) (/ (+ (tanh x) (tanh y)) (+ 1 (* (tanh x) (tanh y))))])

(define-ruleset*
 htrig-expand-sound-rev
 (hyperbolic sound)
 #:type ([x real] [y real])
 [sinh-undef-rev (* 2 (sinh x)) (- (exp x) (exp (neg x)))]
 [cosh-undef-rev (* 2 (cosh x)) (+ (exp x) (exp (neg x)))]
 [diff-cosh-rev (* 2 (* (sinh (/ (+ x y) 2)) (sinh (/ (- x y) 2)))) (- (cosh x) (cosh y))]
 [diff-sinh-rev (* 2 (* (cosh (/ (+ x y) 2)) (sinh (/ (- x y) 2)))) (- (sinh x) (sinh y))]
 [cosh-diff-rev (- (* (cosh x) (cosh y)) (* (sinh x) (sinh y))) (cosh (- x y))]
 [sinh-diff-rev (- (* (sinh x) (cosh y)) (* (cosh x) (sinh y))) (sinh (- x y))]
 [tanh-1/2-rev (/ (sinh x) (+ (cosh x) 1)) (tanh (/ x 2))]
 [tanh-2-rev (/ (* 2 (tanh x)) (+ 1 (* (tanh x) (tanh x)))) (tanh (* 2 x))]
 [sinh-1/2-rev (/ (sinh x) (sqrt (* 2 (+ (cosh x) 1)))) (sinh (/ x 2))]
 [cosh-1/2-rev (sqrt (/ (+ (cosh x) 1) 2)) (cosh (/ x 2))]
 [sinh-2-rev (* 2 (* (sinh x) (cosh x))) (sinh (* 2 x))]
 [cosh-2-rev (+ (* (sinh x) (sinh x)) (* (cosh x) (cosh x))) (cosh (* 2 x))]
 [sinh-sum-rev (+ (* (sinh x) (cosh y)) (* (cosh x) (sinh y))) (sinh (+ x y))]
 [tanh-sum-rev (/ (+ (tanh x) (tanh y)) (+ 1 (* (tanh x) (tanh y)))) (tanh (+ x y))]
 [cosh-sum-rev (+ (* (cosh x) (cosh y)) (* (sinh x) (sinh y))) (cosh (+ x y))]
 [sum-cosh-rev (* 2 (* (cosh (/ (+ x y) 2)) (cosh (/ (- x y) 2)))) (+ (cosh x) (cosh y))]
 [sum-sinh-rev (* 2 (* (sinh (/ (+ x y) 2)) (cosh (/ (- x y) 2)))) (+ (sinh x) (sinh y))])

(define-ruleset* htrig-expand
                 (hyperbolic)
                 #:type ([x real] [y real])
                 [tanh-1/2* (tanh (/ x 2)) (/ (- (cosh x) 1) (sinh x))]
                 [tanh-1/2*-rev (/ (- (cosh x) 1) (sinh x)) (tanh (/ x 2))])

(define-ruleset* htrig-expand-fp-safe
                 (hyperbolic fp-safe sound)
                 #:type ([x real])
                 [sinh-neg (sinh (neg x)) (neg (sinh x))]
                 [sinh-0 (sinh 0) 0]
                 [sinh-0-rev 0 (sinh 0)]
                 [cosh-neg (cosh (neg x)) (cosh x)]
                 [cosh-0 (cosh 0) 1]
                 [cosh-0-rev 1 (cosh 0)])

(define-ruleset* htrig-expand-fp-safe-rev
                 (hyperbolic fp-safe sound)
                 #:type ([x real])
                 [cosh-neg-rev (cosh x) (cosh (neg x))]
                 [sinh-neg-rev (neg (sinh x)) (sinh (neg x))])

(define-ruleset* ahtrig-expand-sound
                 (hyperbolic sound)
                 #:type ([x real])
                 [asinh-def (asinh x) (log (+ x (sqrt (+ (* x x) 1))))]
                 [acosh-def (acosh x) (log (+ x (sqrt (- (* x x) 1))))]
                 [atanh-def (atanh x) (/ (log (/ (+ 1 x) (- 1 x))) 2)]
                 [sinh-asinh (sinh (asinh x)) x]
                 [sinh-acosh (sinh (acosh x)) (sqrt (- (* x x) 1))]
                 [sinh-atanh (sinh (atanh x)) (/ x (sqrt (- 1 (* x x))))]
                 [cosh-asinh (cosh (asinh x)) (sqrt (+ (* x x) 1))]
                 [cosh-acosh (cosh (acosh x)) x]
                 [cosh-atanh (cosh (atanh x)) (/ 1 (sqrt (- 1 (* x x))))]
                 [tanh-asinh (tanh (asinh x)) (/ x (sqrt (+ 1 (* x x))))]
                 [tanh-acosh (tanh (acosh x)) (/ (sqrt (- (* x x) 1)) x)]
                 [tanh-atanh (tanh (atanh x)) x])

(define-ruleset* ahtrig-expand-sound-simplify-rev
                 (hyperbolic sound)
                 #:type ([x real])
                 [asinh-def-rev (log (+ x (sqrt (+ (* x x) 1)))) (asinh x)]
                 [atanh-def-rev (/ (log (/ (+ 1 x) (- 1 x))) 2) (atanh x)]
                 [acosh-def-rev (log (+ x (sqrt (- (* x x) 1)))) (acosh x)]
                 [sinh-acosh-rev (sqrt (- (* x x) 1)) (sinh (acosh x))]
                 [tanh-asinh-rev (/ x (sqrt (+ 1 (* x x)))) (tanh (asinh x))]
                 [cosh-asinh-rev (sqrt (+ (* x x) 1)) (cosh (asinh x))]
                 [sinh-atanh-rev (/ x (sqrt (- 1 (* x x)))) (sinh (atanh x))]
                 [tanh-acosh-rev (/ (sqrt (- (* x x) 1)) x) (tanh (acosh x))]
                 [cosh-atanh-rev (/ 1 (sqrt (- 1 (* x x)))) (cosh (atanh x))])

(define-ruleset* ahtrig-expand
                 (hyperbolic)
                 #:type ([x real])
                 [asinh-2 (acosh (+ (* 2 (* x x)) 1)) (* 2 (asinh x))]
                 [acosh-2 (acosh (- (* 2 (* x x)) 1)) (* 2 (acosh x))])

(define-ruleset* ahtrig-expand-rev
                 (hyperbolic)
                 #:type ([x real])
                 [acosh-2-rev (* 2 (acosh x)) (acosh (- (* 2 (* x x)) 1))])

;; Sound because it's about soundness over real numbers
(define-ruleset* compare-reduce
                 (bools simplify fp-safe-nan sound)
                 #:type ([x real] [y real])
                 [lt-same (< x x) (FALSE)]
                 [gt-same (> x x) (FALSE)]
                 [lte-same (<= x x) (TRUE)]
                 [gte-same (>= x x) (TRUE)]
                 [not-lt (not (< x y)) (>= x y)]
                 [not-gt (not (> x y)) (<= x y)]
                 [not-lte (not (<= x y)) (> x y)]
                 [not-gte (not (>= x y)) (< x y)])

(define-ruleset* branch-reduce
                 (branches simplify fp-safe sound)
                 #:type ([a bool] [b bool] [x real] [y real])
                 [if-true (if (TRUE) x y) x]
                 [if-false (if (FALSE) x y) y]
                 [if-same (if a x x) x]
                 [if-not (if (not a) x y) (if a y x)]
                 [if-if-or
                  (if a
                      x
                      (if b x y))
                  (if (or a b) x y)]
                 [if-if-or-not
                  (if a
                      x
                      (if b y x))
                  (if (or a (not b)) x y)]
                 [if-if-and
                  (if a
                      (if b x y)
                      y)
                  (if (and a b) x y)]
                 [if-if-and-not
                  (if a
                      (if b y x)
                      y)
                  (if (and a (not b)) x y)])
