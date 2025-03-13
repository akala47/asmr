;;NOTE: For Racket Operations
#lang racket

(require asmr/asm-block)

(asm-block
 (registers [(‘rax ‘())
 (‘rbx ‘())])
 [(mov ‘rax 0)
 (mov ‘rbx 20)
 (add ‘rax (* 4 5)) ; evaluate nested sexp
 (sub ‘rax ‘rbx)])

;;____EXPECTED_OUTPUT____

;;+------------+--------+
;;| Register | Value |
;;+------------+--------+
;;| 'rax | 0 |
;;| 'rbx | 20 |
;;+------------+--------+