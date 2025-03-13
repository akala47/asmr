
![alt text](logo.png "ASMR Logo")

# ASMR - Assembly-Racket
ASMR is a domain-specific language built in Racket for students
to learn low-level programming concepts using a high-level syntax. 
Designed as a teaching tool, it provides architecture-agnostic assembly programming with integrated 
debugging capabilities.

## Key Features of ASMR
- **Architecture Agnostic**: Setting custom registers enables emulating multiple platforms
- **Debugging(Print)**: Built-in `print-registers` command for register/memory inspection
- **Flexible Register Configuration**: Ability to define set of registers per assembly block
(treated as an environment/world)
- **Syntax Validation**: Compile-time checks for register usage and instruction validity

## Overview of Design Choices
One of the key driving factors behind design choices seen in _ASMR_ is
the fact that it is intended to be a teaching language that emulates 
Assembly. Some design decisions that were made were:
- Having a Macros within a main Macro structure to define an environment
with defined registers. The current version of our plan intends to have 
a main macro(`asm-block`) which specifies the world(similar to `big-bang`)
which then uses the `registers` macro to define the registers within that world.
- Enabling `racket math-interops` within our DSL to extend functionality
to allow storing values like `hex-values` in registers. 

### Validation System
The DSL performs compile-time checks for:
- Undeclared register usage
- Label reference consistency
- Instruction syntax validity
- Operand type matching

**Example Error Detection:**
```racket
;;--------EXAMPLE VALIDATION--------
;; (asm-block
    (registers [])
    [(mov ‘rpx 10)])
;; -> Error: register ‘rpx not defined
```


## Implementation Roadmap
1. **Phase 1**
   - Data definition for registers
   - Finalizing syntax and types
   - Pretty print(`print-registers`)

2. **Phase 2**
   - Support for basic instructions
   - Implementing top level asm-block macro
   - Standardisation to emulate different architectures

3. **Phase 3**
   - Clean up and testing 
   - Export to native assembly(`optimistic!`)
   - Possible interactive environment and GUI implementation with a stepper


The first phase of our implementation should set the foundations for
the development of our language. The first phase, thus involves 
fixating on the syntax and types and finalizing supported patterns.
This includes the definitions for data like `registers` as well as
patterns like `add` and `jmp`. Furthermore, an initial, barebones 
implementation of `print-registers` would enable us to potentially
use it in testing during the other phases of our implementation.   
Some ideas that are up for evaluation
during this phase are:
- lexical scoping for `<label>`
- the scope of `registers` and mutability constraints(if any) for 
different
data definitions
<br>

The second phase of our implementation would include working on basic 
functionality support and implementing the top level `asm-block` macro
to enable performing operations on defined registers. During the last part
of this phase, we aim to provide support to emulate different standard architecture
register sets in a pre-defined manner.

Lastly, during the third phase of our implementation, clean up and extensive testing
will take up an initial part of the time, after which additional features like
exporting to native assembly and a possible GUI implementation to display
a stepper would be worked upon, as much as time permits.


## Comparison with Traditional Assemblers
| Feature               | Traditional Assemblers | ASMR DSL         |
|-----------------------|------------------------|------------------|
| Architecture Binding  | Platform-specific      | Agnostic         |
| Debug Capabilities    | External tools         | Integrated       |
| Register Management   | Fixed set              | Customizable     |
| Syntax Checking       | Basic                  | Comprehensive    |




## Syntax Specification
### Core BNF Grammar
```racket
<asm-block> ::= (asm-block (registers [<register>]) [<stmts>])
<stmt> ::= (mov <dest> <src>)          ; Move operation
         | (add|sub|imul <dest> <src>) ; Arithmetic operations
         | (cmp <dest> <src>)          ; Compare operation
         | (jmp|je|jne <label>)        ; Control flow
         | (label <label>)             ; Label definition
         | (print-registers)           ; Debug command

<register> ::= <reg> <imm> | <reg> '()
<registers> ::= <register> | <register> <registers>
<reg> ::= <symbol>
<dest> ::= <reg>
<src> ::= <imm> | <reg>
```

## Usage Example
```racket
(asm-block
  (registers [('rax '()) ('rbx '()) ('rcx '())])
  [(cmp 'rax 'rbx)
   (jne 'error)
   (mov 'rcx 10)
   (jmp 'exit)
   (label 'error)
   (mov 'rax 10)
   (label 'exit)])
```





This design document reflects the current specification as of March 13, 2025.
It shall continue to evolve along the course of this class according to feedback
given by our peers and professors.


Made By:
Aryan Kalaskar
Rishikesh Kanabar
