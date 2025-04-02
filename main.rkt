#lang racket

(require syntax-spec-v3
         (for-syntax syntax/parse racket/list))

(syntax-spec
 (binding-class register)
 (binding-class label)
 
 (host-interface/expression
  (asm-block registers:registers-spec [inst:instruction-spec ...])
  #:binding (scope (import registers) (import inst) ...)
  #'(compile-asm-instructions registers [inst ...])) ;; Compile to Racket using bound variables
  
 (nonterminal/exporting registers-spec
                        (registers ([reg-name:register e:arith-expr] ...))
                        #:binding ((export reg-name) ...))

 (nonterminal arith-expr
              n:number
              (+ e1:arith-expr e2:arith-expr)
              (- e1:arith-expr e2:arith-expr)
              (* e1:arith-expr e2:arith-expr))
  
 (nonterminal/exporting instruction-spec
                        (label lbl:label)
                        #:binding (export lbl) ;; Note: We want to export the label from our nonterminal to machine host interface 
                        (print-registers)
                        (cmp reg1:register reg2:register) ;; Instruction syntax not yet implemented
                        (jne lbl:label)
                        (mov reg:register val:arith-expr)
                        (jmp lbl:label)))

(define (register-set! map reg val) (hash-set! map reg val))
(define (register-get map reg) (hash-ref map reg #f))
(define (display-registers map) (displayln map))
(define registers-map (make-hash))
(define compare-flag #f)

(define (compare v1 v2)
  (begin
    (if (or (false? v1)
            (false? v2))
        (error "Register not valid")
        (if (eq? v1 v2)
            (set! compare-flag #t)
            (set! compare-flag #f)))))

(define-syntax compile-asm-instructions
  (syntax-parser
    [(_ registers [instrs ...])
     #`(begin
         (let* ([registers-list (cadr (syntax->datum #'registers))])
           
           (for ([reg registers-list])
             (let ([reg-name (first reg)] 
                   [reg-value (second reg)])
               (register-set! registers-map reg-name reg-value)))
           
           #,@(for/list ([instr (syntax->list #'(instrs ...))])
                (syntax-parse instr
                  [((~datum cmp) reg1 reg2)
                   #`(let* ([val1 (register-get registers-map 'reg1)]
                            [val2 (register-get registers-map 'reg2)])
                       (compare val1 val2))]
                  [((~datum jne) lbl)
                   #'(displayln "2. Jump if not equal")]
                  [((~datum mov) reg val)
                   #`(register-set! registers-map 'reg val)]
                  [((~datum jmp) lbl)
                   #'(displayln "4. Unconditional jump")]
                  [((~datum label) lbl)
                   #'(displayln "5. Label defined")]
                  [(print-registers) #`(display-registers registers-map)]
                  [_ #'(displayln "7. Unknown")]))

           (void)))]))


(asm-block
 (registers [(rax 0) (rbx 10) (rcx 0)])
 [(mov rax 10)
  (cmp rax rbx)
  (print-registers)])
