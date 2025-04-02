#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse racket))





(syntax-spec
 (binding-class label)
 

 #;(host-interface/expression
    (asm-block (run-instructions) [inst:instruction-spec ...])
    #:binding (scope (import inst) ...))

 #;(host-interface/expression
    (asm-block [inst:instruction-spec ...])
    #:binding (scope (import inst) ...)
    #'(#,(compile-asm-instructions inst ...)))  ;; Compile to Racket using bound variables


 (host-interface/expression
  (asm-block [inst:instruction-spec ...])
  #:binding (inst ...)
  #'(begin #,(compile-asm-instructions inst ...)))


 (nonterminal arith-expr
              n:number
              (+ e1:arith-expr e2:arith-expr)
              (- e1:arith-expr e2:arith-expr)
              (* e1:arith-expr e2:arith-expr))

 (nonterminal/exporting register
                        var:racket-var
                        #:binding (scope (bind var)))
  
 (nonterminal instruction-spec
                        (cmp reg1:register reg2:register)
                        #:binding (scope (import reg1) (import reg2))

                        (mov reg:register val:arith-expr)
                        #:binding (scope (bind reg) val)

                        (print-registers)))



(define-syntax compile-asm-instructions
  (syntax-parser
    (display ("final-macro not implemented"))))

;;(define reg-lam (λ (

(asm-block
 (cmp rax rbx)
 ;(jne error)
 (mov rcx 10)
 ;(jmp exit)
 ;(label error)
 (mov rax 10)
 ;(label exit)
 (print-registers))