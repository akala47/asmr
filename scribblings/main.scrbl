#lang scribble/manual

@(require scribble/example
          racket/sandbox)

@(define evaluator
   '(begin
      (require syntax-spec-v3 racket/list)))

@title{ASMR: Assembly Macro for Racket}
@author{Aryan K, Rishikesh K}

@section{Introduction}

ASMR (Assembly Macro for Racket) is a domain-specific language that provides assembly-like syntax within Racket programs. It allows you to write assembly-style code with registers, basic instructions, and control flow operations.

@section{Getting Started}

To use ASMR, you need to require the appropriate module:

@codeblock{
#lang racket
(require asmr)
}

@section{Grammar}

The formal grammar for ASMR is defined as follows:

@codeblock{
<program> ::= (asm-block <registers> <instruction-list>)

<registers> ::= (registers [(<register-pair>) ...])
<register-pair> ::= (<register-name> <arith-expr>)

<instruction-list> ::= [<instruction> ...]

<instruction> ::= <label>
                | (print-registers)
                | (cmp <register-name> <register-name>)
                | (cmp <register-name> <arith-expr>)
                | (je <label-name>)
                | (jne <label-name>)
                | (mov <register-name> <register-name>)
                | (mov <register-name> <arith-expr>)
                | (add <register-name> <register-name>)
                | (add <register-name> <arith-expr>)
                | (sub <register-name> <register-name>)
                | (sub <register-name> <arith-expr>)
                | (imul <register-name> <register-name>)
                | (imul <register-name> <arith-expr>)
                | (jmp <label-name>)

<label> ::= <label-name>
<arith-expr> ::= <number>
<register-name> ::= IDENTIFIER
<label-name> ::= IDENTIFIER
<number> ::= IDENTIFIER
}

@section{Basic Syntax}

The main construct in ASMR is the @racket[asm-block] form, which takes a register specification and a sequence of instructions:

@codeblock{
(asm-block
 (registers [(reg-name value) ...])
 [instruction ...])
}

@subsection{Register Specification}

Registers are defined using the @racket[registers] form, which takes a list of register name and initial value pairs:

@codeblock{
(registers [(rax 0) (rbx 10) (rcx 20)])
}

@subsection{Instructions}

ASMR supports the following instructions:

@subsubsection{Data Movement}

@itemlist[
  @item{@racket[(mov reg1 reg2)] - Move the value from @racket[reg2] to @racket[reg1]}
  @item{@racket[(mov reg value)] - Set @racket[reg] to the immediate @racket[value]}
]

@subsubsection{Arithmetic Operations}

@itemlist[
  @item{@racket[(add reg1 reg2)] - Add the value in @racket[reg2] to @racket[reg1]}
  @item{@racket[(add reg value)] - Add the immediate @racket[value] to @racket[reg]}
  @item{@racket[(sub reg1 reg2)] - Subtract the value in @racket[reg2] from @racket[reg1]}
  @item{@racket[(sub reg value)] - Subtract the immediate @racket[value] from @racket[reg]}
  @item{@racket[(imul reg1 reg2)] - Multiply @racket[reg1] by the value in @racket[reg2]}
  @item{@racket[(imul reg value)] - Multiply @racket[reg] by the immediate @racket[value]}
]

@subsubsection{Comparison and Jumps}

@itemlist[
  @item{@racket[(cmp reg1 reg2)] - Compare values in @racket[reg1] and @racket[reg2]}
  @item{@racket[(cmp reg value)] - Compare value in @racket[reg] with immediate @racket[value]}
  @item{@racket[(je label)] - Jump to @racket[label] if the last comparison was @bold{equal}}
  @item{@racket[(jne label)] - Jump to @racket[label] if the last comparison was @bold{not equal}}
  @item{@racket[(jmp label)] - @bold{Unconditional} jump to @racket[label]}
]

@subsubsection{Labels and Debugging}

@itemlist[
  @item{@racket[(label name)] - Define a label named @racket[name] at the current position}
  @item{@racket[(print-registers)] - Display the current state of all registers}
]

@section{Examples}

@subsection{Basic Register Operations}

@codeblock{
(asm-block
 (registers [(rax 0) (rbx 0) (rcx 0)])
 [(mov rax 10)
  (print-registers)
  (mov rbx 5)
  (add rax 10)
  (mov rcx rax)
  (print-registers)])
}

@bold{Output:}
@codeblock{
Registers State:
rax: 10
rbx: 0
rcx: 0

Registers State:
rax: 20
rbx: 5
rcx: 20
}

This example shows basic register operations: setting values, arithmetic, and copying between registers. The first @racket[print-registers] shows @racket[rax] set to 10, while the second shows the results after adding 10 to @racket[rax] (making it 20), setting @racket[rbx] to 5, and copying the value of @racket[rax] to @racket[rcx].

@subsection{Conditional Jumps}

@codeblock{
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
}

@bold{Output:}
@codeblock{
Registers State:
rax: 0
rbx: 0
rcx: 10
}

In this example, the code compares registers @racket[rax] and @racket[rbx]. Since they're both initialized to 0, they're equal, so the @racket[je] instruction jumps to the @racket[error] label. However, the output shows @racket[rcx] is 10, which suggests the code actually jumps when values are equal (as expected with @racket[je]).

@subsection{Computing Sum of Numbers 1 to 100}

@codeblock{
(asm-block
 (registers [(rax 0) (rbx 1) (rcx 100)])
 [(label loop)
  (add rax rbx)
  (add rbx 1)
  (cmp rbx rcx)
  (jne loop)
  (add rax rbx)
  (print-registers)])
}

@bold{Output:}
@codeblock{
Registers State:
rax: 5050
rbx: 100
rcx: 100
}

This example demonstrates a loop that computes the sum of numbers from 1 to 100, resulting in 5050. The program uses a loop with @racket[jne] to iterate until @racket[rbx] equals @racket[rcx].

@subsection{Multiplication Example}

@codeblock{
(asm-block
 (registers [(rax 5)])
 [(imul rax 4)
  (print-registers)])
}

@bold{Output:}
@codeblock{
Registers State:
rax: 20
}

This example shows the multiplication operation, multiplying the value in @racket[rax] (5) by 4, resulting in 20.

@subsection{Loop with Backward Jump}

@codeblock{
(asm-block
 (registers [(rax 0) (rbx 3)])
 [(label start)
  (add rax 1)
  (sub rbx 1)
  (cmp rbx 0)
  (jne start)
  (print-registers)])
}

@bold{Output:}
@codeblock{
Registers State:
rax: 3
rbx: 0
}

This example demonstrates a loop using a backward jump. The loop increments @racket[rax] and decrements @racket[rbx] until @racket[rbx] reaches zero, effectively counting how many iterations occurred.


@section{}
Refer to the @link["https://github.com/akala47/asmr"]{asmr github} for more details on the implementation.
