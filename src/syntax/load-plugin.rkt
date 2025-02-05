#lang racket
(require setup/getinfo
         racket/runtime-path)
(require "../config.rkt"
         "platform.rkt"
         "types.rkt")

(provide load-herbie-builtins
         load-herbie-plugins
         make-debug-context)

;; Builtin plugins
(define-runtime-module-path bool-plugin "../platforms/bool.rkt")
(define-runtime-module-path binary32-plugin "../platforms/binary32.rkt")
(define-runtime-module-path binary64-plugin "../platforms/binary64.rkt")
(define-runtime-module-path fallback-plugin "../platforms/fallback.rkt")

;; Builtin platforms
(define-runtime-module-path c-platform "../platforms/libm.rkt")
(define-runtime-module-path default-platform "../platforms/default.rkt")
(define-runtime-module-path math-platform "../platforms/math.rkt")

; Automatically loads default representations and platforms
(define (load-herbie-builtins)
  ;; Load in all plugins
  ;; Warning: the order here is important!
  (dynamic-require bool-plugin #f)
  (dynamic-require binary64-plugin #f)
  (dynamic-require binary32-plugin #f)
  (dynamic-require fallback-plugin #f)
  ;; Load all platforms
  (dynamic-require c-platform #f)
  (dynamic-require default-platform #f)
  (dynamic-require math-platform #f)
  ; activate the required platform
  (*active-platform* (get-platform (*platform-name*)))
  (activate-platform! (*active-platform*)))

(define (load-herbie-plugins)
  (load-herbie-builtins)
  ; search packages for herbie plugins
  (for ([dir (find-relevant-directories '(herbie-plugin))])
    (define info
      (with-handlers ([exn:fail:filesystem? (const false)])
        (get-info/full dir)))
    (define value (info 'herbie-plugin (const false)))
    (when value
      (with-handlers ([exn:fail:filesystem:missing-module? void])
        (dynamic-require value #f))))
  ; load in "loose" plugins
  (for ([path (in-list (*loose-plugins*))])
    (dynamic-require path #f))
  ; activate the actual requred platform
  (*active-platform* (get-platform (*platform-name*)))
  (activate-platform! (*active-platform*)))

(define (make-debug-context vars)
  (load-herbie-builtins)
  (define repr (get-representation 'binary64))
  (context vars repr (map (const repr) vars)))
