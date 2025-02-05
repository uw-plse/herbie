#lang racket

(require math/bigfloat
         math/base)
(require "../utils/common.rkt"
         "../syntax/types.rkt"
         "../utils/errors.rkt")

(provide ulp-difference
         ulps->bits
         midpoint
         random-generate
         </total
         <=/total
         value->string
         value->json
         json->value
         real->repr
         repr->real)

(define (ulp-difference x y repr)
  (define ->ordinal (representation-repr->ordinal repr))
  (+ 1 (abs (- (->ordinal y) (->ordinal x)))))

;; Returns the midpoint of the representation's ordinal values,
;; not the real-valued midpoint
(define (midpoint p1 p2 repr)
  ((representation-ordinal->repr repr) (floor (/ (+ ((representation-repr->ordinal repr) p1)
                                                    ((representation-repr->ordinal repr) p2))
                                                 2))))

(define (ulps->bits x)
  (real->double-flonum (log x 2)))

(define (random-generate repr)
  ((representation-ordinal->repr repr) (random-bits (representation-total-bits repr))))

(define (=/total x1 x2 repr)
  (define ->ordinal (representation-repr->ordinal repr))
  (define special? (representation-special-value? repr))
  (or (= (->ordinal x1) (->ordinal x2))
      (if (real? x1) ; Infinities are considered special values for real reprs for some reason
          (and (nan? x1) (nan? x2))
          (and (special? x1) (special? x2)))))

(define (</total x1 x2 repr)
  (define special? (representation-special-value? repr))
  (define ->ordinal (representation-repr->ordinal repr))
  (cond
    [(and (real? x1) (real? x2))
     (cond
       [(nan? x1) #f]
       [(nan? x2) #t]
       [else (< x1 x2)])]
    [else
     (cond
       [(special? x1) #f]
       [(special? x2) #t]
       [else (< (->ordinal x1) (->ordinal x2))])]))

(define (<=/total x1 x2 repr)
  (or (</total x1 x2 repr) (=/total x1 x2 repr)))

(define (value->json x repr)
  (match x
    [(? real?)
     (match x
       [(? rational?) x]
       [(or -inf.0 -inf.f) (hash 'type "real" 'value "-inf")]
       [(or +inf.0 +inf.f) (hash 'type "real" 'value "+inf")]
       [(or +nan.0 +nan.f) (hash 'type "real" 'value "NaN")])]
    [_
     (hash 'type
           (~a (representation-name repr))
           'ordinal
           (~a ((representation-repr->ordinal repr) x)))]))

(define (json->value x repr)
  (match x
    [(? real?) x]
    [(? hash?)
     (match (hash-ref x 'type)
       ["real"
        (match (hash-ref x 'value)
          ["-inf" -inf.0]
          ["+inf" +inf.0]
          ["NaN" +nan.0]
          [_ +nan.0])]
       [_ ((representation-ordinal->repr repr) (string->number (hash-ref x 'ordinal)))])]))

(define (value->string n repr)
  ;; Prints a number with relatively few digits
  (define ->bf (representation-repr->bf repr))
  (define <-bf (representation-bf->repr repr))
  ;; Linear search because speed not an issue
  (let loop ([precision 16])
    (if (> precision (*max-mpfr-prec*))
        (begin
          (warn 'value-to-string #:url "faq.html#value-to-string" "Could not uniquely print ~a" n)
          n)
        (parameterize ([bf-precision precision])
          (define bf (->bf n))
          (if (=/total n (<-bf bf) repr)
              (match (bigfloat->string bf)
                ["-inf.bf" "-inf.0"]
                ["+inf.bf" "+inf.0"]
                ["+nan.bf" "+nan.0"]
                [x x])
              (loop (+ precision 4))))))) ; 2^4 > 10

(define (real->repr x repr)
  ((representation-bf->repr repr) (bf x)))

(define (repr->real x repr)
  (match x
    [(? boolean?) x]
    [_ (bigfloat->real ((representation-repr->bf repr) x))]))
