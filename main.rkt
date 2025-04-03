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
                        #:binding (export lbl)
                        (print-registers)
                        (cmp reg1:register reg2:register)
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

           (for ([inst instr-list]
                 [i (in-naturals)])
             (syntax-case inst ()
               [(label-id lbl)
                (and (identifier? #'label-id) (eq? 'label (syntax->datum #'label-id)))
                (hash-set! label-index-map (syntax->datum #'lbl) i)]
               [_ (void)]))
              
           (for ([reg registers-list])
             (let ([reg-name (first reg)] 
                   [reg-value (second reg)])
               (register-set! registers-map reg-name reg-value)))

           (loop go ([pc 0])
                 
                 (when (< pc (length instr-list))
                   (let* ([inst `(list-ref instr-list pc)])
                     #`(syntax-parse inst
                         [((~datum cmp) reg1 reg2)
                          (let* ([val1 (register-get registers-map 'reg1)]
                                 [val2 (register-get registers-map 'reg2)])
                            (compare val1 val2)
                            (go (add1 pc))
                            (displayln "CHUT"))
                          ]
                         [((~datum jne) lbl)
                          (let* ([lbl-index (hash-ref label-index-map 'lbl)])
                            (if compare-flag
                                (go (add1 pc))
                                (go lbl-index))
                            (displayln "CHUT"))]
                         [((~datum mov) reg val)
                          (begin
                            (register-set! registers-map 'reg val)
                            (go (add1 pc))
                            (displayln "CHUT"))]
                         [((~datum jmp) lbl)
                          (let* ([lbl-index (hash-ref label-index-map 'lbl)])
                            (go lbl-index)
                            (displayln "CHUT"))]
                         [((~datum label) lbl) (go (add1 pc))]
                         [(print-registers)
                          (begin
                            (display-registers registers-map)
                            (go (add1 pc))
                            (displayln "CHUT"))]
                         [_ 
                          (displayln "Unknown instruction")
                          (go (add1 pc))]))))
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
