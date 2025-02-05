#lang racket

(require "syntax/types.rkt"
         "utils/errors.rkt"
         (submod "syntax/types.rkt" internals)
         (submod "syntax/syntax.rkt" internals)
         (submod "syntax/platform.rkt" internals)
         (submod "core/rules.rkt" internals))

(provide define-type
         define-representation
         define-operator-impl
         define-operator
         define-ruleset
         define-ruleset*
         register-ruleset!
         register-operator-impl!
         register-representation!
         register-representation-alias!
         register-conversion-generator!
         register-generator!
         register-operator!
         (struct-out representation)
         get-representation
         warn
         define-platform
         get-platform
         register-platform!
         platform-union
         platform-intersect
         platform-subtract
         platform-filter)
