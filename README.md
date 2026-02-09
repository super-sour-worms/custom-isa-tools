## About
This is a suite for my custom CPU Instruction Set Architecture, that i designed during boring math lessons at technikum.

I found myself getting frustrated with the existing design echoing early ARM issues. Improving it would have meant removing the unique elements that set it apart, essentially creating RISC-V clone, so **i am abandoning this project**.

My ISA drew strong influences from RISC-V and early ARM (arm7tdmi to be precise). Initial design was supposed to have variable-size register windows, but this feature was making architecture too sophisticated.

## Goals:
I wanted to design software, and hardware platform all by myself. Even though this project is frozen, i still want to write an OS, i will just use RISC-V when urge to write an OS returns.

1. Design base ISA, write assembler, and emulator (project was abandoned during this stage)
2. ~~Write the BASIC Programming Language interpreter in my assembly language to try out design~~
3. ~~Write a compiler, or LLVM backend~~
4. ~~Write an OS, but thats a different project~~

## Usage
### Assembling
```sh
isa-tools assemble assembler-file.asm --output binary_file.bin
```
Default output file is _a.bin_.

### Executing assembler code
```sh
isa-tools execute a.asm
```

I did not implement running binaries directly.
