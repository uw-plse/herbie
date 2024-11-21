#lang racket

(require "../plugin.rkt")

; universal boolean opertaions
(define-platform boolean-platform
                 #:literal [bool 1]
                 #:default-cost 1
                 #:if-cost 1
                 TRUE
                 FALSE
                 not
                 and
                 or)

(define-platform arith-platform
                 #:literal [binary32 32]
                 [PI.f32 32]
                 [E.f32 32]
                 [INFINITY.f32 32]
                 [NAN.f32 32]
                 [neg.f32 64]
                 [+.f32 64]
                 [-.f32 64]
                 [*.f32 128]
                 [/.f32 320]
                 [==.f32 128]
                 [!=.f32 128]
                 [>.f32 128]
                 [<.f32 128]
                 [>=.f32 128]
                 [<=.f32 128]
                 [fabs.f32 128]
                 [fmax.f32 128]
                 [fmin.f32 128]
                 [sqrt.f32 128])

(define arith (platform-union boolean-platform arith-platform))

(register-platform! 'arith arith)

                   

