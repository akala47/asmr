;;NOTE: For Racket Operations
#lang racket

(require asmr/asm-block)

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

;;____EXPECTED OUTPUT____

;;| Register : Value |
;;+------------+--------+
;;| 'rax : 0
;;| 'rbx : 0 
;;| 'rcx : 10
;;+------------+--------+