#lang racket

(require (only-in xml write-xexpr xexpr?)
         (only-in fpbench core->tex supported-by-lang?)
         json)
(require "../core/rules.rkt"
         "../syntax/sugar.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../core/bsearch.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt"
         "../core/points.rkt"
         "../core/programs.rkt"
         "common.rkt")
(provide render-history
         render-json)

(define (split-pcontext pcontext splitpoints alts ctx)
  (define preds (splitpoints->point-preds splitpoints alts ctx))

  (for/list ([pred preds])
    (define-values (pts* exs*)
      (for/lists (pts exs) ([(pt ex) (in-pcontext pcontext)] #:when (pred pt)) (values pt ex)))

    ;; TODO: The (if) here just corrects for the possibility that we
    ;; might have sampled new points that include no points in a given
    ;; regime. Instead it would be best to continue sampling until we
    ;; actually have many points in each regime. That would require
    ;; breaking some abstraction boundaries right now so we haven't
    ;; done it yet.
    (if (null? pts*)
        pcontext
        (mk-pcontext pts* exs*))))

(struct interval (alt-idx start-point end-point expr))

(define (interval->string ival repr)
  (define start (interval-start-point ival))
  (define end (interval-end-point ival))
  (string-join (list (if start
                         (format "~a < " (value->string start repr))
                         "")
                     (~a (interval-expr ival))
                     (if (equal? end +nan.0)
                         ""
                         (format " < ~a" (value->string end repr))))))

(define (splice-proof-step step)
  (let/ec k
          (let loop ([expr step]
                     [loc '()])
            (match expr
              [(list 'Rewrite=> rule sub)
               (define loc* (reverse loc))
               (k 'Rewrite=> rule loc* (location-do loc* step (λ _ sub)))]
              [(list 'Rewrite<= rule sub)
               (define loc* (reverse loc))
               (k 'Rewrite<= rule loc* (location-do loc* step (λ _ sub)))]
              [(approx spec impl)
               (loop spec (cons 1 loc))
               (loop impl (cons 2 loc))]
              [(hole prec spec) (loop spec (cons 1 loc))]
              [(list op args ...)
               (for ([arg (in-list args)]
                     [i (in-naturals 1)])
                 (loop arg (cons i loc)))]
              [_ (void)]))
          (k 'Goal #f '() step)))

(define (altn-errors altn pcontext pcontext2 ctx)
  (define repr (context-repr ctx))
  (define repr-bits (representation-total-bits repr))
  (define err (errors-score (errors (alt-expr altn) pcontext ctx)))
  (define err2 (errors-score (errors (alt-expr altn) pcontext2 ctx)))
  (values (format-accuracy err repr-bits #:unit "%")
          (format "~a on training set" (format-accuracy err2 repr-bits #:unit "%"))))

(define (expr->fpcore expr ctx #:ident [ident #f])
  (list 'FPCore
        (context-vars ctx)
        (let loop ([expr expr])
          (match expr
            [(? symbol?) expr]
            [(? number?) expr]
            [(? literal?) (literal-value expr)]
            [(approx spec impl) (loop impl)]
            [(hole precision spec) (loop spec)]
            [(list op args ...) (cons op (map loop args))]))))

(define (mixed->fpcore expr ctx)
  (define expr*
    (let loop ([expr expr])
      (match expr
        [(? symbol?) expr]
        [(? number?) expr]
        [(? literal?) (literal-value expr)]
        [(approx _ impl) (loop impl)]
        [(hole precision spec) (loop spec)]
        [`(if ,cond ,ift ,iff)
         `(if ,(loop cond)
              ,(loop ift)
              ,(loop iff))]
        [`(,(? impl-exists? impl) ,args ...)
         ; use the FPCore operator without rounding properties
         (define args* (map loop args))
         (define vars (impl-info impl 'vars))
         (define pattern
           (match (impl-info impl 'fpcore)
             [(list '! _ ... body) body]
             [body body]))
         (replace-vars (map cons vars args*) pattern)]
        [`(,op ,args ...) `(,op ,@(map loop args))])))
  `(FPCore ,(context-vars ctx) ,expr*))

;; HTML renderer for derivations
(define/contract (render-history altn pcontext pcontext2 ctx)
  (-> alt? pcontext? pcontext? context? (listof xexpr?))
  (match altn
    [(alt prog 'start (list) _)
     (define-values (err err2) (altn-errors altn pcontext pcontext2 ctx))
     (list `(li (p "Initial program " (span ((class "error") [title ,err2]) ,err))
                (div ((class "math")) "\\[" ,(program->tex prog ctx) "\\]")))]

    [(alt prog 'add-preprocessing `(,prev) _)
     ;; TODO message to user is? proof later
     `(,@(render-history prev pcontext pcontext2 ctx) (li "Add Preprocessing"))]

    [(alt _ `(regimes ,splitpoints) prevs _)
     (define intervals
       (for/list ([start-sp (cons (sp -1 -1 #f) splitpoints)]
                  [end-sp splitpoints])
         (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp))))
     (define repr (context-repr ctx))

     `((li ((class "event")) "Split input into " ,(~a (length prevs)) " regimes")
       (li ,@(apply append
                    (for/list ([entry prevs]
                               [idx (in-naturals)]
                               [new-pcontext (split-pcontext pcontext splitpoints prevs ctx)]
                               [new-pcontext2 (split-pcontext pcontext2 splitpoints prevs ctx)])
                      (define entry-ivals
                        (filter (λ (intrvl) (= (interval-alt-idx intrvl) idx)) intervals))
                      (define condition
                        (string-join (map (curryr interval->string repr) entry-ivals) " or "))
                      `((h2 (code "if " (span ((class "condition")) ,condition)))
                        (ol ,@(render-history entry new-pcontext new-pcontext2 ctx))))))
       (li ((class "event")) "Recombined " ,(~a (length prevs)) " regimes into one program."))]

    [(alt prog `(taylor ,loc ,pt ,var) `(,prev) _)
     (define core (mixed->fpcore prog ctx))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Taylor expanded in " ,(~a var) " around " ,(~a pt))
           (div ((class "math"))
                "\\[\\leadsto "
                ,(core->tex core #:loc (and loc (cons 2 loc)) #:color "blue")
                "\\]")))]

    [(alt prog `(simplify ,loc ,input ,proof) `(,prev) _)
     (define-values (err err2) (altn-errors altn pcontext pcontext2 ctx))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li ,(if proof
                (render-proof proof pcontext ctx)
                ""))
       (li (p "Simplified" (span ((class "error") [title ,err2]) ,err))
           (div ((class "math")) "\\[\\leadsto " ,(program->tex prog ctx #:loc loc) "\\]")))]

    [(alt prog `initial-simplify `(,prev) _)
     (define-values (err err2) (altn-errors altn pcontext pcontext2 ctx))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Initial simplification" (span ((class "error") [title ,err2]) ,err))
           (div ((class "math")) "\\[\\leadsto " ,(program->tex prog ctx) "\\]")))]

    [(alt prog `final-simplify `(,prev) _)
     (define-values (err err2) (altn-errors altn pcontext pcontext2 ctx))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li (p "Final simplification" (span ((class "error") [title ,err2]) ,err))
           (div ((class "math")) "\\[\\leadsto " ,(program->tex prog ctx) "\\]")))]

    [(alt prog `(rr ,loc ,input ,proof) `(,prev) _)
     (define-values (err err2) (altn-errors altn pcontext pcontext2 ctx))
     `(,@(render-history prev pcontext pcontext2 ctx)
       (li ,(if proof
                (render-proof proof pcontext ctx)
                ""))
       (li (p "Applied rewrites" (span ((class "error") [title ,err2]) ,err))
           (div ((class "math")) "\\[\\leadsto " ,(program->tex prog ctx #:loc loc) "\\]")))]))

(define (render-proof proof pcontext ctx)
  `(div ((class "proof"))
        (details (summary "Step-by-step derivation")
                 (ol ,@(for/list ([step proof])
                         (define-values (dir rule loc expr) (splice-proof-step step))
                         ;; need to handle mixed real/float expressions
                         (define-values (err prog)
                           (cond
                             [(impl-prog? expr) ; impl program?
                              (values (format-accuracy (errors-score (errors expr pcontext ctx))
                                                       (representation-total-bits (context-repr ctx)))
                                      (program->fpcore expr ctx))]
                             [else (values "N/A" (mixed->fpcore expr ctx))]))
                         ; the proof
                         (if (equal? dir 'Goal)
                             ""
                             `(li ,(let ([dir (match dir
                                                ['Rewrite<= "right to left"]
                                                ['Rewrite=> "left to right"])])
                                     `(p (code ([title ,dir]) ,(~a rule))
                                         (span ((class "error")) ,err)))
                                  (div ((class "math"))
                                       "\\[\\leadsto "
                                       ,(core->tex prog #:loc (and loc (cons 2 loc)) #:color "blue")
                                       "\\]"))))))))

(define (render-json altn pcontext pcontext2 ctx)
  (define repr (context-repr ctx))
  (define-values (err err2)
    (if (impl-prog? (alt-expr altn))
        (values (errors-score (errors (alt-expr altn) pcontext ctx))
                (errors-score (errors (alt-expr altn) pcontext2 ctx)))
        (values "N/A" "N/A")))

  (match altn
    [(alt prog 'start (list) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "start")
            (error . ,err)
            (training-error . ,err2))]

    [(alt prog `(regimes ,splitpoints) prevs _)
     (define intervals
       (for/list ([start-sp (cons (sp -1 -1 #f) splitpoints)]
                  [end-sp splitpoints])
         (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp))))

     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "regimes")
            (conditions . ,(for/list ([entry prevs]
                                      [idx (in-naturals)])
                             (let ([entry-ivals (filter (λ (intrvl) (= (interval-alt-idx intrvl) idx))
                                                        intervals)])
                               (map (curryr interval->string repr) entry-ivals))))
            (prevs . ,(for/list ([entry prevs]
                                 [new-pcontext (split-pcontext pcontext splitpoints prevs ctx)]
                                 [new-pcontext2 (split-pcontext pcontext2 splitpoints prevs ctx)])
                        (render-json entry new-pcontext new-pcontext2 ctx))))]

    [(alt prog `(taylor ,loc ,pt ,var) `(,prev) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "taylor")
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (pt . ,(~a pt))
            (var . ,(~a var))
            (loc . ,loc)
            (error . ,err)
            (training-error . ,err2))]

    [(alt prog `(simplify ,loc ,input ,proof) `(,prev) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "simplify")
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (proof . ,(if proof
                          (render-proof-json proof pcontext ctx)
                          (json-null)))
            (loc . ,loc)
            (error . ,err)
            (training-error . ,err2))]

    [(alt prog `initial-simplify `(,prev) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "initial-simplify")
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (error . ,err)
            (training-error . ,err2))]

    [(alt prog `final-simplify `(,prev) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "final-simplify")
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (error . ,err)
            (training-error . ,err2))]

    [(alt prog `(rr ,loc ,input ,proof) `(,prev) _)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "rr")
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (proof . ,(if proof
                          (render-proof-json proof pcontext ctx)
                          (json-null)))
            (loc . ,loc)
            (error . ,err)
            (training-error . ,err2))]

    [(alt prog 'add-preprocessing `(,prev) preprocessing)
     `#hash((program . ,(fpcore->string (expr->fpcore prog ctx)))
            (type . "add-preprocessing")
            (prev . ,(render-json prev pcontext pcontext2 ctx))
            (error . ,err)
            (training-error . ,err2)
            (preprocessing . ,(map (curry map symbol->string) preprocessing)))]))

(define (render-proof-json proof pcontext ctx)
  (for/list ([step proof])
    (define-values (dir rule loc expr) (splice-proof-step step))
    (define err
      (if (impl-prog? expr)
          (errors-score (errors expr pcontext ctx))
          "N/A"))

    `#hash((error . ,err)
           (program . ,(fpcore->string (expr->fpcore expr ctx)))
           (direction . ,(match dir
                           ['Rewrite<= "rtl"]
                           ['Rewrite=> "ltr"]
                           ['Goal "goal"]))
           (rule . ,(~a rule))
           (loc . ,loc))))
