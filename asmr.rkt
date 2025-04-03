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
                        ;; Label Definition
                        (label lbl:label)
                        #:binding (export lbl)

                        ;; Pretty Print Registers
                        (print-registers)

                        ;; CMP Instruction
                        (cmp reg1:register reg2:register)
                        (cmp reg:register val:arith-expr)

                        ;; Conditional Jump (If Equals) Instruction
                        (je lbl:label)

                        ;; Conditional Jump (If Not Equals) Instruction
                        (jne lbl:label)

                        ;; MOV Instruction
                        (mov reg1:register reg2:register)
                        (mov reg:register val:arith-expr)

                        ;; ADD Instruction
                        (add reg1:register reg2:register)
                        (add reg:register val:arith-expr)

                        ;; SUB Instruction
                        (sub reg1:register reg2:register)
                        (sub reg:register val:arith-expr)

                        ;; IMUL Instruction
                        (imul reg1:register reg2:register)
                        (imul reg:register val:arith-expr)

                        ;; Unconditional Jump Instruction
                        (jmp lbl:label)))

;; Hash map to store register values
(define registers-map (make-hash))

;; Hash map to store label indices
(define label-index-map (make-hash))

;; Flag indicating the result of the last comparison operation
(define compare-flag #f)

;; Function to set a register's value in the registers hash map
(define (register-set! reg val) (hash-set! registers-map reg val))

;; Function to get a register's value from the registers hash map
(define (register-get reg) (hash-ref registers-map reg #f))

;; Function to display the current state of all registers
(define (display-registers)
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

;; Function to compare two values and set the comparison flag accordingly.
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
         ;; Process register definitions and and instructions
         (let* ([registers-list (cadr (syntax->datum #'registers))]
                [instr-list (list 'instrs ...)])

           ;; Process labels to map them to instruction indices
           (for ([inst instr-list]
                 [i (in-naturals)])
             (match inst
               [(list 'label lbl)
                (hash-set! label-index-map lbl i)]
               [_ (void)]))

           ;; Initialize registers with their specified initial values
           (for ([reg registers-list])
             (let ([reg-name (first reg)] 
                   [reg-value (second reg)])
               (register-set! reg-name reg-value)))
           
           ;; Helper function for executing comparison instructions
           (define (exec-cmp reg val)
             (if (symbol? val)
                 (compare (register-get reg) (register-get val))
                 (compare (register-get reg) val)))

           ;; Helper function for executing arithmetic operations
           (define (exec-arithmetic op dest src)
             (let ([dest-val (register-get dest)]
                   [src-val (if (symbol? src) 
                                (register-get src)
                                src)])
               (register-set! dest ((case op
                                      ['+ +]
                                      ['- -]
                                      ['* *]) dest-val src-val))))
           
           (let loop ([pc 0])
             (when (< pc (length instr-list))
               (let ([inst (list-ref instr-list pc)])
                 (match inst
                   [(list 'mov reg val)
                    (register-set! reg 
                                   (if (symbol? val) 
                                       (register-get val) 
                                       val))
                    (loop (+ pc 1))]

                   [(list 'add dest src)
                    (exec-arithmetic '+ dest src)
                    (loop (+ pc 1))]

                   [(list 'sub dest src)
                    (exec-arithmetic '- dest src)
                    (loop (+ pc 1))]

                   [(list 'imul dest src)
                    (exec-arithmetic '* dest src)
                    (loop (+ pc 1))]

                   [(list 'cmp reg val)
                    (exec-cmp reg val)
                    (loop (+ pc 1))]
                   
                   [(list 'je lbl)
                    (loop 
                     (if compare-flag 
                         (hash-ref label-index-map lbl) 
                         (+ pc 1)))]

                   [(list 'jne lbl)
                    (loop 
                     (if (not compare-flag) 
                         (hash-ref label-index-map lbl) 
                         (+ pc 1)))]

                   [(list 'jmp lbl)
                    (loop 
                     (hash-ref label-index-map lbl))]

                   [(list 'label _)
                    ;; No-op during execution
                    (loop (+ pc 1))]

                   [print-registers
                    (begin
                      (display-registers)
                      (loop (+ pc 1)))]

                   ;; Unreachable
                   [_  (error "Unknown instruction" inst)]))))
           (void)))]))

;; Testing - E2E

(module+ test
  (require rackunit)

  ;; Captures and returns all output printed to the current output port
  (define (capture-output thunk)
    (let ([output-port (open-output-string)])
      (parameterize ([current-output-port output-port])
        (thunk))
      (get-output-string output-port)))
  
  ;; Register Initialization Tests
  
  (test-case "Simple Register Initialization"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 5) (rbx 10)])
                        [(print-registers)])))])
               (check-true (string-contains? output "rax: 5"))
               (check-true (string-contains? output "rbx: 10"))))

  (test-case "Register Initialization with Zero Values"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0) (rbx 0) (rcx 0)])
                        [(print-registers)])))])
               (check-true (string-contains? output "rax: 0"))
               (check-true (string-contains? output "rbx: 0"))
               (check-true (string-contains? output "rcx: 0"))))

  (test-case "Register Initialization with Negative Values"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax -5) (rbx -10)])
                        [(print-registers)])))])
               (check-true (string-contains? output "rax: -5"))
               (check-true (string-contains? output "rbx: -10"))))

  ;; MOV Instruction Tests
  
  (test-case "MOV Instruction with Immediate Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0) (rbx 0)])
                        [(mov rax 42)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 42"))
               (check-true (string-contains? output "rbx: 0"))))

  (test-case "MOV Instruction with Register Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0) (rbx 0)])
                        [(mov rax 42)
                         (mov rbx rax)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 42"))
               (check-true (string-contains? output "rbx: 42"))))

  (test-case "MOV Instruction Overwrite Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0) (rbx 0)])
                        [(mov rax 42)
                         (mov rbx 10)
                         (mov rbx rax)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 42"))
               (check-true (string-contains? output "rbx: 42"))))

  (test-case "MOV Instruction with Negative Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0)])
                        [(mov rax -25)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: -25"))))

  ;; ADD Instruction Tests
  
  (test-case "ADD Instruction with Immediate Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10)])
                        [(add rax 5)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 15"))))

  (test-case "ADD Instruction with Register Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10) (rbx 20)])
                        [(add rax rbx)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 30"))
               (check-true (string-contains? output "rbx: 20"))))

  (test-case "ADD Instruction with Negative Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10)])
                        [(add rax -15)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: -5"))))

  (test-case "ADD Instruction with Zero"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10) (rbx 0)])
                        [(add rax rbx)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 10"))))

  ;; SUB Instruction Tests
  
  (test-case "SUB Instruction with Immediate Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 20)])
                        [(sub rax 5)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 15"))))

  (test-case "SUB Instruction with Register Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 30) (rbx 10)])
                        [(sub rax rbx)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 20"))
               (check-true (string-contains? output "rbx: 10"))))

  (test-case "SUB Instruction with Negative Result"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10) (rbx 20)])
                        [(sub rax rbx)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: -10"))
               (check-true (string-contains? output "rbx: 20"))))

  (test-case "SUB Instruction with Negative Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10)])
                        [(sub rax -5)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 15"))))

  ;; IMUL Instruction Tests

  (test-case "IMUL Instruction with Immediate Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 5)])
                        [(imul rax 4)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 20"))))

  (test-case "IMUL Instruction with Register Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 3) (rbx 7)])
                        [(imul rax rbx)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 21"))
               (check-true (string-contains? output "rbx: 7"))))

  (test-case "IMUL Instruction with Negative Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax -4)])
                        [(imul rax 3)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: -12"))))

  (test-case "IMUL Instruction Resulting in Zero"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0) (rbx 10)])
                        [(imul rax rbx)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 0"))
               (check-true (string-contains? output "rbx: 10"))))
  
  (test-case "IMUL Instruction with Both Negative Values"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax -6) (rbx -7)])
                        [(imul rax rbx)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 42"))
               (check-true (string-contains? output "rbx: -7"))))

 
  (test-case "IMUL Instruction by One"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 8)])
                        [(imul rax 1)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 8"))))

  (test-case "IMUL Instruction for Squaring a Value"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 5)])
                        [(imul rax rax)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 25"))))

 
  
  ;; CMP and Conditional Jump Tests

  (test-case "CMP Instruction with Equal Values"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10) (rbx 10) (rcx 0)])
                        [(cmp rax rbx)
                         (je equal)
                         (mov rcx 1)
                         (jmp exit)
                         (label equal)
                         (mov rcx 2)
                         (label exit)
                         (print-registers)])))])
               (check-true (string-contains? output "rcx: 2"))))

  (test-case "CMP Instruction with Unequal Values"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10) (rbx 20) (rcx 0)])
                        [(cmp rax rbx)
                         (jne not_equal)
                         (mov rcx 1)
                         (jmp exit)
                         (label not_equal)
                         (mov rcx 2)
                         (label exit)
                         (print-registers)])))])
               (check-true (string-contains? output "rcx: 2"))))

  (test-case "JE Instruction When Equal"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10) (rbx 10) (rcx 0)])
                        [(cmp rax rbx)
                         (je equal)
                         (mov rcx 1)
                         (jmp exit)
                         (label equal)
                         (mov rcx 2)
                         (label exit)
                         (print-registers)])))])
               (check-true (string-contains? output "rcx: 2"))))

  (test-case "JE Instruction When Not Equal"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10) (rbx 20) (rcx 0)])
                        [(cmp rax rbx)
                         (je equal)
                         (mov rcx 1)
                         (jmp exit)
                         (label equal)
                         (mov rcx 2)
                         (label exit)
                         (print-registers)])))])
               (check-true (string-contains? output "rcx: 1"))))

  (test-case "JNE Instruction When Equal"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10) (rbx 10) (rcx 0)])
                        [(cmp rax rbx)
                         (jne not_equal)
                         (mov rcx 1)
                         (jmp exit)
                         (label not_equal)
                         (mov rcx 2)
                         (label exit)
                         (print-registers)])))])
               (check-true (string-contains? output "rcx: 1"))))

  (test-case "JNE Instruction When Not Equal"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10) (rbx 20) (rcx 0)])
                        [(cmp rax rbx)
                         (jne not_equal)
                         (mov rcx 1)
                         (jmp exit)
                         (label not_equal)
                         (mov rcx 2)
                         (label exit)
                         (print-registers)])))])
               (check-true (string-contains? output "rcx: 2"))))

  ;; Unconditional Jump Tests
  
  (test-case "JMP Instruction"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0)])
                        [(mov rax 1)
                         (jmp skip)
                         (mov rax 2)
                         (label skip)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 1"))))

  (test-case "JMP to Forward Label"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0) (rbx 0)])
                        [(mov rax 1)
                         (jmp forward)
                         (mov rax 2)
                         (label forward)
                         (mov rbx 3)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 1"))
               (check-true (string-contains? output "rbx: 3"))))

  (test-case "JMP to Backward Label (Loop)"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0) (rbx 3)])
                        [(label start)
                         (add rax 1)
                         (sub rbx 1)
                         (cmp rbx 0)
                         (jne start)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 3"))
               (check-true (string-contains? output "rbx: 0"))))

  ;; Complex Program Tests
  
  (test-case "Random Program"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0) (rbx 0) (rcx 0)])
                        [(cmp rax rbx)
                         (jne error)
                         (mov rcx 10)
                         (jmp exit)
                         (label error)
                         (mov rax 10)
                         (label exit)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 0"))
               (check-true (string-contains? output "rbx: 0"))
               (check-true (string-contains? output "rcx: 10"))))

  (test-case "Compute Sum of Numbers 1 to 100"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 0) (rbx 1) (rcx 100)])
                        [(label loop)
                         (add rax rbx)
                         (add rbx 1)
                         (cmp rbx rcx)
                         (jne loop)
                         (add rax rbx)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 5050")) ; Answer: 1 + 2 + ... + 100 = 5050
               (check-true (string-contains? output "rbx: 100"))
               (check-true (string-contains? output "rcx: 100"))))

  ;; Edge Cases and Error Handling
  
  (test-case "Empty Instruction List"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10)])
                        [])))])
               (check-equal? output "")))

  (test-case "Program with Only Labels"
             (let ([output
                    (capture-output
                     (lambda ()
                       (asm-block
                        (registers [(rax 10)])
                        [(label start)
                         (label middle)
                         (label end)
                         (print-registers)])))])
               (check-true (string-contains? output "rax: 10"))))

  #;(test-case "Unbound Register"
               (let ([output
                      (capture-output
                       (lambda ()
                         (asm-block
                          (registers [])
                          [(mov rax 10)])))])
                 (check-true (string-contains? output "mov: not bound as register in: rax")))))