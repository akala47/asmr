#lang racket

;; NOTE: For Racket Operations

(require asmr/asm-block)

(asm-block
   (cmp rax rbx)
   (jne error)
   (mov rcx 10)
   (jmp exit)
   (label error)
   (mov rax 10)
   (label exit)
   (print-registers))

;;____EXPECTED OUTPUT____

;; +------------+--------+
;; | Register   : Value  |
;; +------------+--------+
;; | 'rax       : 0      |
;; | 'rbx       : 0      |
;; | 'rcx       : 10     |
;; +------------+--------+