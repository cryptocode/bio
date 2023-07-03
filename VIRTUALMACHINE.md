The Bio Virtual Machine (BVM) is a bytecode virtual machine for the Bio programming language, a Lisp dialect.

It is tailored specifically towards executing Bio code efficiently.

# Model

The VM executes a stream of instructions that operates on a stack and affects registers.

The main register is the PC - the program counter - which is logically an index into the bytecode. Execution of instructions moves the PC as they're executed. Non-jumping instructions move the PC to the next instruction, while absolute or PC-relative jump instructions causes a new PC to be calculated.

The stack is growable and is used to pass arguments to function and to hold results from function calls and value-producing instructions.

Instructions may have zero, one or many operands.

The execution of a BVM program is simply:

```
repeat
    calculate PC
    fetch instruction and any operands at current PC
    execute instruction
```

The execution loop ends when the instruction stream ends, or when the `exit` instruction is evaluated.

# Compilation

Bytecode is produced by `(compile expr)`, such as `(var compiled-add (compile '(* a 2)))`

The resulting expression can be evaluated with eval: `(eval compiled-add)`

The expression can also be disassembled with `(disassemble compiled-add)` which outputs the assembly mnemonics:

```bash
push var a
push f64 2
mul
```

# Instruction set format

instruction ::= opcode arg*
opcode ::= enum(u8)
arg ::= value
value ::= f64 | sym | list | map | intrinsic | ...
