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
                [instr-list (syntax->list #'(instrs ...))])

           (for ([inst instr-list] [i (in-naturals)])
             (syntax-case inst ()
               [(label-id lbl)
                (and (identifier? #'label-id) (eq? 'label (syntax->datum #'label-id)))
                (hash-set! label-index-map (syntax->datum #'lbl) i)]
               [_ (void)]))
              
           (for ([reg registers-list])
             (let ([reg-name (first reg)] 
                   [reg-value (second reg)])
               (register-set! registers-map reg-name reg-value)))

           (let loop ([pc 0])
             (when (< pc (length instr-list))
               (let ([inst (list-ref instr-list pc)])
                 #`(syntax-parse inst
                     [((~datum cmp) reg1 reg2)
                      (let* ([val1 (register-get registers-map 'reg1)]
                             [val2 (register-get registers-map 'reg2)])
                        (compare val1 val2))
                      (loop (+ pc 1))]
                     [((~datum jne) lbl)
                      (if compare-flag
                          (loop (+ pc 1))
                          (loop (hash-ref label-index-map 'lbl)))]
                     [((~datum mov) reg val)
                      (register-set! registers-map 'reg val)
                      (loop (+ pc 1))]
                     [((~datum jmp) lbl)
                      (loop (hash-ref label-index-map 'lbl))]
                     [((~datum label) lbl) (loop (+ pc 1))]
                     [(print-registers)
                      (display-registers registers-map)
                      (loop (+ pc 1))]
                     [_ 
                      (displayln "Unknown instruction")
                      (loop (+ pc 1))]))))

           (void)))]))


(asm-block
 (registers [(rax 0) (rbx 0) (rcx 0)])
 [(cmp rax rbx)        ; 0
  (jne error)          ; 1
  (mov rcx 10)         ; 2
  (jmp exit)           ; 3
  (label error)        ; 4
  (mov rax 10)         ; 5
  (label exit)         ; 6
  (print-registers)])  ; 7
