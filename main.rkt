#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse))



(syntax-spec
 (binding-class register-spec)

 (host-interface/expression
    (registers r:register-spec)
    #:binding r
    #'(compile-registers r))

  (nonterminal set-registers
    [name:id init:racket-expr]))

(define-syntax compile-registers
  (syntax-parser
    [(_ [name:id init:racket-expr] ...)
     #'(begin
         (define reg-hash (hash 'name init ...))
         (define (print-registers)
           (for ([(k v) reg-hash])
             (printf "~a: ~a\n" k v))))]))



