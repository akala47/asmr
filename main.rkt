#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse racket/list))


(binding-class label #:reference-compiler mutable-reference-compiler)

(syntax-spec
  (host-interface/expression
    (asm-block registers:registers-spec [inst:instruction-spec ...])
    #:binding (scope registers (import inst) ...)
    #'(compile-asm-instructions registers inst ...)) ;; Compile to Racket using bound variables
  
  (nonterminal registers-spec
    (registers ([(quote reg-name) (quote initial-value)] ...)))

  (nonterminal print-spec
    #'(print-registers registers))

  (nonterminal/exporting instruction-spec
    (label lbl:label)
    #:binding (export lbl) ;; Note: We want to export the label from our nonterminal to machine host interface 
    (cmp (quote reg1) (quote reg2)) ;; Instruction syntax not yet implemented
    (jne lbl:label)
    (mov (quote reg) val:number)
    (jmp lbl:label)))

(define-syntax compile-asm-instructions
  (syntax-parser
    (displayln "not yet implemented")))

(define-syntax print-registers
  (syntax-parser
    (displayln "not yet implemented")))

;; Example Usage
#;(asm-block
  (registers [('rax '()) ('rbx '()) ('rcx '())])
  [(cmp 'rax 'rbx)
   (jne 'error)
   (mov 'rcx 10)
   (jmp 'exit)
   (label 'error)
   (mov 'rax 10)
   (label 'exit)])