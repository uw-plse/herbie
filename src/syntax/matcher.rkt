;; Minimal pattern matcher/substituter for S-expressions

#lang racket

(provide merge-bindings
         pattern-match
         pattern-substitute)

;; Unions two bindings. Returns #f if they disagree.
(define (merge-bindings binding1 binding2)
  (and binding1
       binding2
       (let/ec quit
         (for/fold ([binding binding1]) ([(k v) (in-dict binding2)])
           (dict-update binding
                        k
                        (λ (x)
                          (if (equal? x v)
                              v
                              (quit #f)))
                        v)))))

;; Pattern matcher that returns a substitution or #f.
;; A substitution is an association list of symbols and expressions.
(define (pattern-match pattern expr)
  (match* (pattern expr)
    [((? number?) _) (and (equal? pattern expr) '())]
    [((? symbol?) _) (list (cons pattern expr))]
    [((list phead prest ...) (list head rest ...))
     (and (equal? phead head)
          (= (length prest) (length rest))
          (for/fold ([bindings '()])
                    ([pat (in-list prest)]
                     [term (in-list rest)])
            (merge-bindings bindings (pattern-match pat term))))]
    [(_ _) #f]))

(define (pattern-substitute pattern bindings)
  ; pattern binding -> expr
  (match pattern
    [(? number?) pattern]
    [(? symbol?) (dict-ref bindings pattern)]
    [(list phead pargs ...) (cons phead (map (curryr pattern-substitute bindings) pargs))]))
