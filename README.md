![alt text](logo.png "ASMR Logo")

# ASMR - Assembly-Racket

ASMR is a domain-specific language built in Racket for students
to learn low-level programming concepts using a high-level syntax.
Designed as a teaching tool, it provides architecture-agnostic assembly programming with integrated
debugging capabilities.

## Key Features of ASMR

- **Architecture Agnostic**: Setting custom registers enables emulating multiple platforms
- **Debugging (Print)**: Built-in `print-registers` command for register/memory inspection
- **Flexible Register Configuration**: Ability to define set of registers per assembly block
  (treated as an environment/world)
- **Syntax Validation**: Compile-time checks for register usage and instruction validity

## Overview of Design Choices

One of the key driving factors behind design choices seen in _ASMR_ is
the fact that it is intended to be a teaching language that emulates
Assembly. Some design decisions that were made were:

- Having a Macros within a main Macro structure to define an environment
  with defined registers. The current version of our plan intends to have
  a main macro(`asm-block`) which specifies the world (similar to `big-bang`)
  which then uses the `registers` macro to define the registers within that world.

### Validation System

The DSL performs compile-time checks for:

- Undeclared register usage
- Label reference consistency
- Instruction syntax validity
- Operand type matching

**Example Error Detection:**

```racket
;;--------EXAMPLE VALIDATION--------
   (asm-block
    (registers [])
    [(mov rpx 10)])
;; -> mov: not bound as register in: rpx
```

## Comparison with Traditional Assemblers

| Feature              | Traditional Assemblers | ASMR DSL      |
| -------------------- | ---------------------- | ------------- |
| Architecture Binding | Platform-specific      | Agnostic      |
| Debug Capabilities   | External tools         | Integrated    |
| Register Management  | Fixed set              | Customizable  |
| Syntax Checking      | Basic                  | Comprehensive |

## Syntax Specification

### Core BNF Grammar

```racket
<program> ::= "asm-block" <registers> <instruction-list>

<registers> ::= "registers" "(" <register-pair>* ")"
<register-pair> ::= "(" <register-name> <arith-expr> ")"

<instruction-list> ::= "[" <instruction>* "]"

<instruction> ::= <label>
                | "print-registers"
                | "cmp" <register-name> <register-name>
                | "cmp" <register-name> <arith-expr>
                | "je" <label-name>
                | "jne" <label-name>
                | "mov" <register-name> <register-name>
                | "mov" <register-name> <arith-expr>
                | "add" <register-name> <register-name>
                | "add" <register-name> <arith-expr>
                | "sub" <register-name> <register-name>
                | "sub" <register-name> <arith-expr>
                | "imul" <register-name> <register-name>
                | "imul" <register-name> <arith-expr>
                | "jmp" <label-name>

<label> ::= "label" <label-name>

<arith-expr> ::= <number>

<register-name> ::= IDENTIFIER
<label-name> ::= IDENTIFIER
<number> ::= INTEGER
```

## Usage Example

```racket
(asm-block
  (registers [(rax 0) (rbx 0) (rcx 0)])
  [(cmp rax rbx)
   (jne error)
   (mov rcx 10)
   (jmp exit)
   (label error)
   (mov rax 10)
   (label exit)])
```

This design document reflects the current specification as of April 3, 2025.
It shall continue to evolve along the course of this class according to feedback
given by our peers and professors.

Made By:

- Aryan Kalaskar
- Rishikesh Kanabar
