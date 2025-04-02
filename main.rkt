#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse racket))

(syntax-spec
 ;; Binding class for registers
 ;(binding-class register)

 (host-interface/expression
  (asm-block inst:instruction-spec ...)
  #:binding (inst ...)
  #'(compile-asm-instructions inst ...))


 (nonterminal arith-expr
   n:number
   e:racket-expr)

 
 (nonterminal/exporting register
   r:racket-var
   #:binding (export r))

 
 (nonterminal instruction-spec
   (cmp reg1:register reg2:register)
   #:binding (scope(import reg1) (import reg2))  ; Import registers

   (mov reg:register val:arith-expr)
   #:binding (scope (import reg) val)  ; Import register

   (print-registers)))

;; TODO

;; Use a hash table to store registers
(define registers (make-hash))



;; Runtime functions for register operations
(define (register-set! reg val) (hash-set! registers reg val))
(define (register-get reg) (hash-ref registers reg #f))
(define (display-registers) (displayln registers))

(define (add-64-registers)
  (begin (register-set! 'rax 0)
         (register-set! 'rbx 0)
         (register-set! 'rcx 0)
         (register-set! 'rdx 0)
         (register-set! 'r8 0)
         (register-set! 'r9 0)))
(define compare-flag #f)


;; Macro to expand instructions without using `define`
(define-syntax compile-asm-instructions
  (syntax-parser
    [(_ inst ...)
     #`(begin
           (add-64-registers)
           #,@(for/list ([instr (syntax->list #'(inst ...))])
                (syntax-parse instr
                  [(mov reg val) #'(register-set! 'reg val)]
                  [(cmp reg1 reg2) #'(compare (register-get 'reg1) (register-get 'reg2))]
                  [(print-registers) #'(display-registers)])))]))

(define (compare val1 val2)
  (begin (if (or (false? val1 #f)
                 (false? val2 #f))
             (error "Register not valid")
             (if (eq? val1 val2)
                 (set! compare-flag #t)
                 (set! compare-flag #f)) )))

;; Example usage
(asm-block
 (mov rax 10)
 (mov rbx 20)
 (cmp rax rbx)
 (print-registers))


(asm-block
 (mov rax 20)
 (mov rbx 10)
 ;(cmp rzx rbx)
 (print-registers))
