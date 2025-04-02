#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse racket))

(syntax-spec
 ;; Binding class for registers
 (binding-class register)

 (host-interface/expression
  (asm-block inst:instruction-spec ...)
  #:binding (inst ...)
  #'(compile-asm-instructions inst ...))


 (nonterminal arith-expr
   n:number
   e:racket-expr)

 
 (nonterminal/exporting register-spec
   r:register
   #:binding (export r))

 
 (nonterminal instruction-spec
   (cmp reg1:register-spec reg2:register-spec)
   #:binding (scope(import reg1) (import reg2))  ; Import registers

   (mov reg:register-spec val:arith-expr)
   #:binding (scope (import reg) val)  ; Import register

   (print-registers)))

;; TODO
(define-syntax compile-asm-instructions
  (syntax-parser
    [(_ inst ...)
     #'(length `(syntax->list inst ...))]))

;; Example usage
(asm-block
 (mov rax 10)
 (mov rbx 20)
 (cmp rax rbx)
 (print-registers))

(asm-block
 (mov rax 10)
 (mov rbx 20)
 (cmp rax rbx)
 (mov rax 10)
 (print-registers))
