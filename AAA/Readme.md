# An Accumulator Simulator and Compiler
#### Sean Clemmer for CMSC-22200

## Terminology

- **AAA** - An Accumulator Architecture, pronounced "A"
- **AAS** - An Accumulator Simulator, pronounced "ass"
- **AAC** - An Accumulator Compiler, pronounced "ack"

## Overview

The project is divided into two main areas, the simulator and the compiler: **AAS**  is a simulator for a custom ISA, **AAA**, based on a simple accumulator and inspired by MIPS. **AAC** is a simple two-pass compiler/assembler for the accumulator architecture supporting a few modest directives. Also included are a couple of example programs, `rot13.s` (ROT-13 encoding) and `99bottles.s` (the "99 Bottles of Beer" song), which together use every instruction supported by the system.

_NB:_ **AAC** depends on **PEGGED**, a parser generator for D. I've included **PEGGED** in the source, and you can read more about it on [its GitHub project page](https://github.com/PhilippeSigaud/Pegged).

## Instruction Format

Although you'll never deal with them directly, **AAA** instructions are 32-bits long:

- `OP` - 5-bit  opcode field
- `AUX` - 11-bit auxiliary data field (currently unused)
- `DATA` - 16-bit data field

Or graphically:

    0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
    +--------------+-----------------------------+-----------------------------------------------+
    | OP           | AUX                         | DATA                                          |
    +--------------+-----------------------------+-----------------------------------------------+

## **AAS** Hardware

**AAS** utilizes the following registers:

- `IR1` - First instruction register (contains `OP` and `AUX`)
- `IR2` - Second instruction register (contains `DATA`)
- `IP` - Instruction pointer (to memory)
- `XP` - Saved instruction pointer (for `call`/`ret`)
- `ACC` - The Accumulator
- `MAR` - Memory address register
- `MDR` - Memory data register

**AAS** also maintains two condition bits:

- `ZF` - Zero flag (set when `ACC == 0`)
- `HALT` - Halt flags (set on `halt`)

Main memory is divided into 16-bit "slots" and may be bit-addressed by programs (with undefined behavior if access is not on a slot boundary). Instructions should occupy two contiguous slots in memory.

## **AAC** Assembly

**AAC** programs are composed of four types of expressions:

- Comments - "#" to end of line, completely ignored
- Labels - identifier + ":", replaced with offset
- Instructions - identifier + arguments (to EOL), executed by **AAS**
- Macro - "." + directive + arguments (to EOL), explained below

**AAC** supports two special directives:

- `byte` - Writes a byte given in decimal to the current memory slot
- `ascii` - Writes a string delimited by double quotes into memory with one slot for each character

Because there is no distinction between code and data segments, it's usually a good idea to use these macros near the end of your program, because although macro slots are interpreted as `nop`s, they may push actual instructions out of alignment. Also, remember that memory is bit-addressed, not slot-addressed, so you may have to adjust some indices if you are porting programs to the architecture.

## Usage

First, download and submodules and build the project:

    git clone git@github.com:sczizzo/AAA.git && cd AAA
    git submodule init && git submodule update
    make # Use "DFLAGS=-unittest" to include tests

Next, you'll need an assembly program, say `example.s`. **AAC** takes a filename as its first argument and writes the compiled binary to `STDOUT`, so direct that into a file for use with **AAS**, which also takes a filename as its first argument. Altogether, you can compile and run a program like so:

    ./aac example.s > example.bin && ./aas example.bin

Input and output will be directed to `STDIN` and `STDOUT` respectively. Input is taken character-by-character and officially supports only ASCII. Use `^C` to exit.