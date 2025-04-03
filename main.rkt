#lang racket

(require syntax-spec-v3
         loop
         (for-syntax syntax/parse racket/list))

(syntax-spec
 (binding-class register)
 (binding-class label)
 
 (host-interface/expression
  (asm-block registers:registers-spec [inst:instruction-spec ...])
  #:binding (scope (import registers) (import inst) ...)
  #'(compile-asm-instructions registers [inst ...]))
  
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
                        #:binding (export lbl)
                        (print-registers)
                        (cmp reg1:register reg2:register)
                        (je lbl:label)
                        (jne lbl:label)
                        (mov reg:register val:arith-expr)
                        (jmp lbl:label)))

(define (register-set! map reg val) (hash-set! map reg val))
(define (register-get map reg) (hash-ref map reg #f))
(define (display-registers map) (displayln map))
(define registers-map (make-hash))
(define label-index-map (make-hash))
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
         (let* ([registers-list (cadr (syntax->datum #'registers))]
                [instr-list (list 'instrs ...)])
           
           (for ([inst instr-list]
                 [i (in-naturals)])
             (match inst
               [(list 'label lbl)
                (hash-set! label-index-map lbl i)]
               [_ (void)]))
              
           (for ([reg registers-list])
             (let ([reg-name (first reg)] 
                   [reg-value (second reg)])
               (register-set! registers-map reg-name reg-value)))

           (let loop ([pc 0])
             (when (< pc (length instr-list))
               (let ([inst (list-ref instr-list pc)])
                 (match inst
                   [(list 'cmp reg1 reg2)
                    (let* ([val1 (register-get registers-map reg1)]
                           [val2 (register-get registers-map reg2)])
                      (compare val1 val2)
                      (loop (add1 pc)))]

                   [(list 'je lbl)
                    (let* ([lbl-index (hash-ref label-index-map lbl)])
                      (if compare-flag
                          (loop lbl-index)
                          (loop (add1 pc))))]
                   
                   [(list 'jne lbl)
                    (let* ([lbl-index (hash-ref label-index-map lbl)])
                      (if (not compare-flag)
                          (loop lbl-index)
                          (loop (add1 pc))))]

                   [(list 'mov reg val)
                    (begin
                      (register-set! registers-map reg val)
                      (loop (add1 pc)))]

                   [(list 'jmp lbl)
                    (let* ([lbl-index (hash-ref label-index-map lbl)])
                      (loop lbl-index))]

                   [(list 'label _)
                    (loop (add1 pc))]

                   [print-registers
                    (begin
                      (display-registers registers-map)
                      (loop (add1 pc)))]

                   [_ (error "Unknown instruction" inst)]))))
           (void)))]))


(asm-block
 (registers [(rax 0) (rbx 0) (rcx 0)])
 [(cmp rax rbx)        ; 0
  (je error)           ; 1
  (mov rcx 10)         ; 2
  (jmp exit)           ; 3
  (label error)        ; 4
  (mov rax 10)         ; 5
  (label exit)         ; 6
  (print-registers)])  ; 7
