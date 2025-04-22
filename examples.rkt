#lang racket

(require "main.rkt")

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


(asm-block
 (registers [(rax 0) (rbx 0) (rcx 0)])
 [(mov rax 10)
  (print-registers)
  (mov rbx 5)
  (add rax 10)
  (mov rcx rax)
  (print-registers)])
