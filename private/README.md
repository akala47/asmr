# Developer Documentation for ASMR
**ASMR** (Assembly Macro for Racket) is a domain-specific language implemented in Racket that provides an assembly-like programming experience with high-level syntax. This document explains the implementation details, architecture, and design decisions behind ASMR.

## Implementation Overview
The ASMR implementation consists of three main components:

- Syntax Specification: Defines the grammar and syntax rules using Racket's syntax-spec-v3 library

- Runtime System: Provides the execution environment for ASMR programs

- Compilation Macro: Transforms ASMR code into executable Racket code


## File Structure
The implementation is organized as follows:

`main.rkt`: Main implementation file containing syntax specification, runtime system, and compilation macro

`private/README.md`: This developer documentation

`tests/`: Directory containing test cases (currently limited to end to end tests)

`scribblings`: Directory containing `main.scribl` documentation file.

## Syntax Specification
The syntax of ASMR is defined using Racket's [syntax-spec-v3]{https://docs.racket-lang.org/syntax-spec-v3/Reference.html} developed by [Michael Ballantne]{https://github.com/michaelballantyne}. Syntax-spec-v3 provides a declarative way to specify the grammar of our DSL. The specification includes:

### Binding Classes

```lisp
(binding-class register)
(binding-class label)
```

These define the two types of bindings used in ASMR: registers and labels. The binding classes are used to track and validate references to these entities.

### Host Interface
```lisp
(host-interface/expression
 (asm-block registers:registers-spec [inst:instruction-spec ...])
 #:binding (scope (import registers) (import inst) ...)
 #'(compile-asm-instructions registers [inst ...]))
 ```

This defines the main entry point of the DSL, the `asm-block` form, which takes a register specification and a list of instructions.


### Nonterminals
```lisp
(nonterminal/exporting registers-spec
                      (registers ([reg-name:register e:arith-expr] ...))
                      #:binding ((export reg-name) ...))
(nonterminal arith-expr n:number)

(nonterminal/exporting instruction-spec
                      ;; Various instruction forms
                      ...)
                      ```
These define the grammar for register specifications, arithmetic expressions, and instructions. The /exporting variants export bindings for registers and labels.

### Runtime System 
The runtime system manages the state of an ASMR program during execution. It includes:

#### State Management
```lisp
;; Hash map to store register values
(define registers-map (make-hash))

;; Hash map to store label indices
(define label-index-map (make-hash))

;; Flag indicating the result of the last comparison operation
(define compare-flag #f)
```
These global variables maintain the state of the program. The registers-map stores the values of registers, the label-index-map maps label names to instruction indices, and the compare-flag tracks the result of comparison operations.

#### Register Operations
````lisp
(define (register-set! reg val) (hash-set! registers-map reg val))
(define (register-get reg) (hash-ref registers-map reg #f))
````
These functions provide a clean interface for setting and getting register values.

#### Instruction Execution
```lisp
(define (display-registers) ...)
(define (compare v1 v2) ...)
(define (exec-cmp reg val) ...)
(define (exec-arithmetic op dest src) ...)
```
These functions implement the behavior of ASMR instructions. Our model runtime expansion is outlined in detail in the `runtime_code.rkt` file.

#### Compilation Process
The compilation process is handled by the compile-asm-instructions macro, which transforms ASMR code into executable Racket code. The compilation process consists of several steps:

Register Initialization: Extract register definitions and initialize them with their specified values

```lisp
(for ([reg registers-list])
  (let ([reg-name (first reg)] 
        [reg-value (second reg)])
    (register-set! reg-name reg-value)))
    ```
Label Resolution: Process labels to map them to instruction indices

```lisp
(for ([inst instr-list]
      [i (in-naturals)])
  (match inst
    [(list 'label lbl)
     (hash-set! label-index-map lbl i)]
    [_ (void)]))
    ```
Instruction Execution: Execute instructions sequentially, updating the program counter based on jumps

```lisp
(let loop ([pc 0])
  (when (< pc (length instr-list))
    (let ([inst (list-ref instr-list pc)])
      (match inst
        ;; Various instruction handlers
        ...))))
        ```
The macro then uses pattern matching to dispatch to the appropriate handler for each instruction type.

### Instruction Implementation Details

#### Data Movement
```lisp
[(list 'mov reg val)
 (register-set! reg 
               (if (symbol? val) 
                   (register-get val) 
                   val))
 (loop (+ pc 1))]
 ```
The mov instruction sets a register to either an immediate value or the value of another register.

#### Arithmetic Operations
```lisp
[(list 'add dest src)
 (exec-arithmetic '+ dest src)
 (loop (+ pc 1))]

[(list 'sub dest src)
 (exec-arithmetic '- dest src)
 (loop (+ pc 1))]

[(list 'imul dest src)
 (exec-arithmetic '* dest src)
 (loop (+ pc 1))]
 ```
Arithmetic operations are handled by the exec-arithmetic function, which applies the specified operation to the destination and source operands.

#### Comparison and Jumps
```lisp
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
  ```
Comparison instructions set the compare-flag, which is then used by conditional jump instructions to determine whether to jump or continue to the next instruction.

## Testing
The implementation in its current spec includes end to end tests that verify the correctness of all aspects of our DSL. The tests use Racket's rackunit framework and a custom capture-output function to test the behavior of ASMR programs based on the `print-registers` function support.


## Design Decisions and Rationale
### Global State
ASMR uses global variables (registers-map, label-index-map, compare-flag) to maintain program state. This design choice simplifies the implementation. In a future version, these could be encapsulated in a struct or object to allow multiple ASMR programs to run concurrently.

### Syntax Validation
The syntax-spec-v3 library provides compile-time validation of ASMR programs, ensuring that registers are properly defined before use and that labels are valid. This helps catch errors early in the development process.

### Instruction Set
The instruction set of ASMR is deliberately limited to a small set of core assembly operations to improve functionality and provide a base to add support more instructions as features. This makes the language easy to implement while still being powerful enough for educational purposes. 

### Control Flow
ASMR implements control flow using labels and jumps, similar to traditional assembly languages. This approach is simple to implement and understand but requires careful management of the program counter.

## Future Enhancements
- Potential improvements to the ASMR implementation include:
- Memory Model: Add support for memory operations beyond registers
- Floating-Point Support: Extend arithmetic operations to support floating-point numbers
- Additional Instructions: Add more assembly instructions like bitwise operations


## Conclusion
ASMR provides a simple yet powerful assembly-like programming experience within Racket. Its implementation leverages Racket's macro system and syntax specification tools to create a DSL that is both easy to use and educational. This documentation should help developers understand the implementation details and contribute to the project.