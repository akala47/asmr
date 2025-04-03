#lang racket

(define x 10)
(define y 20)

;; Get the current module's namespace
(define ns (module->namespace 'racket))

(define (user-defined-vars)
  (define base-vars (namespace-mapped-symbols (module->namespace 'racket)))
  (filter (lambda (var) (not (member var base-vars))) 
          (namespace-mapped-symbols ns)))

;; List all variables in the current module
(for-each displayln (user-defined-vars))


(define a 2)
(define-syntax gen-l
  (syntax-parser (
(set! a 3)

