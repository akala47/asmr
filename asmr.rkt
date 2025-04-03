#lang racket

(require syntax-spec-v3 (for-syntax syntax/parse racket/list))

(provide (all-defined-out))

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

 (nonterminal arith-expr n:number)
  
 (nonterminal/exporting instruction-spec
                        (label lbl:label)
                        #:binding (export lbl)
                        (print-registers)
                        (cmp reg1:register reg2:register)
                        (je lbl:label)
                        (jne lbl:label)
                        (mov reg1:register reg2:register)
                        (mov reg:register val:arith-expr)
                        (add reg1:register reg2:register)
                        (add reg:register val:arith-expr)
                        (sub reg1:register reg2:register)
                        (sub reg:register val:arith-expr)
                        (jmp lbl:label)))

(define (register-set! reg val) (hash-set! registers-map reg val))
(define (register-get reg) (hash-ref registers-map reg #f))

(define (display-registers registers-map)
  (begin
    (newline)
    (display "Registers State:")
    (newline)
    (for-each 
      (lambda (reg-val-pair)
        (let ([reg (car reg-val-pair)]
              [val (cdr reg-val-pair)])
          (display (format "~a: ~a" reg val))
          (newline)))
      (hash->list registers-map))))

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
               (register-set! reg-name reg-value)))

           (let loop ([pc 0])
             (when (< pc (length instr-list))
               (let ([inst (list-ref instr-list pc)])
                 (match inst
                   [(list 'cmp reg1 reg2)
                    (let* ([val1 (register-get reg1)]
                           [val2 (register-get reg2)])
                      (compare val1 val2)
                      (loop (add1 pc)))]

                   [(list 'add dest src)
                    (if (symbol? src)
                        (let ([dest-val (register-get registers-map dest)]
                              [src-val (register-get registers-map src)])
                          (register-set! registers-map dest (+ dest-val src-val)))
                        (let ([dest-val (register-get dest)])
                          (register-set! dest (+ dest-val src))))
                    (loop (add1 pc))]

                   [(list 'add dest src)
                    (if (symbol? src)
                        (let ([dest-val (register-get registers-map dest)]
                              [src-val (register-get registers-map src)])
                          (register-set! registers-map dest (- dest-val src-val)))
                        (let ([dest-val (register-get registers-map dest)])
                          (register-set! registers-map dest (- dest-val src))))
                    (loop (add1 pc))]
                   
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
                    (if (symbol? val)
                        (let ([source-val (register-get val)])
                          (register-set! reg source-val))
                        (register-set! reg val))
                    (loop (add1 pc))]

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