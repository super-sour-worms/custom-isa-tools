isa-tools assemble a.asm --o asm.bin
isa-tools execute a.asm
isa-tools execute a.bin

## How to execute programs:
You can specify path to file, that will be assembled, and executed as a first
argument, or write assembly code directly to the stdin, and after the EOF
it will be executed.
