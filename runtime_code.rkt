#lang racket

;; State management
(define registers-map (make-hash))
(define label-index-map (make-hash))
(define compare-flag #f)

;; Register operations
(define (register-set! reg val) 
  (hash-set! registers-map reg val))

(define (register-get reg) 
  (hash-ref registers-map reg #f))

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

(define (compare v1 v2)
  (begin
    (if (or (false? v1)
            (false? v2))
        (error "Register not valid")
        (if (eq? v1 v2)
            (set! compare-flag #t)
            (set! compare-flag #f)))))

(define (exec-cmp reg val)
  (if (symbol? val)
      (compare (register-get reg) (register-get val))
      (compare (register-get reg) val)))

;; Arithmetic operations
(define (exec-arithmetic op dest src)
  (let ([dest-val (register-get dest)]
        [src-val (if (symbol? src) 
                     (register-get src)
                     src)])
    (register-set! dest ((case op
                           ['+ +]
                           ['- -]
                           ['* *]) dest-val src-val))))

;; ASM Instructions Implementation
(begin
  (let* ([registers-list '((rax 5) (rbx 10))]
         [instr-list '((add rax rbx) (print-registers))])
    
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
            [(list 'add dest src)
             (exec-arithmetic '+ dest src)
             (loop (+ pc 1))]
            
            [print-registers
             (begin
               (display-registers)
               (loop (+ pc 1)))]
            
            [_ (error "Unknown instruction" inst)]))))
    (void)))