#lang racket

;; Note: Initial version only used to build support for a few basic operations.

(begin
  (define registers
    (make-hash 
          '(["rax" . 0]
            ["rbx" . 0]
            ["rcx" . 0])))

  (displayln "----Initial registers----")
  (for ([(k v) (in-hash registers)])
    (displayln (format "  ~a => ~a" k v)))
  (newline)

  (begin
    (define var (hash-ref registers "rcx" "Error"))

    (hash-set*! registers 
                "rcx" 
                ((lambda (x) (+ x 10)) var)))

  (for ([(k v) (in-hash registers)])
    (displayln (format "  ~a => ~a" k v)))
  (newline)


  (displayln "----FINAL REGISTERS---")
  (for ([(k v) (in-hash registers)])
    (displayln (format "  ~a => ~a" k v))))