#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse racket/list))


(syntax-spec
 (binding-class register)
 (binding-class label)
 
  (host-interface/expression
    (asm-block registers:registers-spec [inst:instruction-spec ...])
    #:binding (scope (import registers) (import inst) ...)
    #'(compile-asm-instructions registers inst ...)) ;; Compile to Racket using bound variables
  
  (nonterminal/exporting registers-spec
    (registers ([reg-name:register e:arith-expr] ...))
    #:binding ((export reg-name) ...))

  (nonterminal arith-expr
    n:number
    (+ e1:arith-expr e2:arith-expr))
  
  (nonterminal/exporting instruction-spec
    (label lbl:label)
    #:binding (export lbl) ;; Note: We want to export the label from our nonterminal to machine host interface 
    (print-registers)
    (cmp reg1:register reg2:register) ;; Instruction syntax not yet implemented
    (jne lbl:label)
    (mov reg:register val:arith-expr)
    (jmp lbl:label)))

(define-syntax compile-asm-instructions
  (syntax-parser
    (displayln "not yet implemented")))


;; Example Usage
(asm-block
  (registers [(rax 0) (rbx 0) (rcx 0)])
  [(cmp rax rbx)
   (jne error)
   (mov rcx 10)
   (jmp exit)
   (label error)
   (mov rax 10)
   (label exit)
   (print-registers)])