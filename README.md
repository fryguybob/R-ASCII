# R-ASCII

This was a project for a computer architecture class that I taught in 2019.
It has a simple RISC ISA where all the instructions are in ASCII in memory.

While silly, it is fun.

The `R-ASCII` executable is a simulator for the ISA and the `adventure-compiler`
executable builds text-based adventure style games from a file that describes
a graph of rooms and their connections.

Example programs include:

| Program            | Description |
| ------------------ | ----------------- |
| `sub-test.ram`     | Simple subtraction test. |
| `bin-sort.ram`     | Sort the bits of a binary number. |
| `quine.ram`        | A R-ASCII quine. |
| `quicksort.ram`    | Quicksort an array of numbers. |
| `rule-io.ram`      | Cellular automata. |
| `adventure.ram`    | Adventure game compiled with `adventure-compiler` defined by `adventure.txt`. |

An example run:

```bash
$ R-ASCII io programs/rule-io.ram 
Enter the number of rows: 10
       █      █                   █                                             
      █ █    █ █                 █ █                                            
     █   █  █   █               █   █                                           
    █ █ █ ██ █ █ █             █ █ █ █                                          
   █      ██      █           █       █                                         
  █ █    ████    █ █         █ █     █ █                                        
 █   █  ██  ██  █   █       █   █   █   █                                       
  █ █ ██████████ █ █ █     █ █ █ █ █ █ █ █                                      
 █    █        █      █   █               █                                     
  █  █ █      █ █    █ █ █ █             █ █         
```

Below is the ISA description I gave to students:

## Problem

Hardware design often needs a suite of specialized tools to analyse and debug
the design.  For this project we will make an emulator for a toy RISC-like ISA
that has some simplifications to make things a bit easier.  With this emulator
we will explore some interesting analysis and a few operating systems concepts.

## R-ASCII ISA

To simplify our ISA will use an encoding with four bytes for most instructions
where each byte is a printable character in ASCII encoding.  The first byte
will serve as opcode (including ALU functionality selection) and the subsequent
bytes will either be registers or immediate values given as hexadecimal digits.
The opcode will dictate if a character is a register or hex digit by position.

The following tables describes all the opcodes and how the
three characters that follow are used, *S* for source register,
*D* for destination register, *R* for read memory at register address,
*W* for write memory at register address, *M* for multiple use register,
*H* for hex digit, and
_ for unused.

| Op |   |   |   | Description |
| --- | --- | --- | --- | --- |
| `L` | *R* | *D* | _ | **Load** from memory at the address in register *R* and put the value in register *D*. |
| `S` | *S* | *W* | _ | **Store** the value from register *S* to memory at the address in register *W*. |
|     |     |     |   |   |
| `+` | *S<sub>1</sub>* | *S<sub>2</sub>* | *D* | **Add** the values from registers *S<sub>1</sub>* and *S<sub>2</sub>* and put the result in register *D*. |
| `-` | *S<sub>1</sub>* | *S<sub>2</sub>* | *D* | **Subtract** the value of registers *S<sub>2</sub>* from *S<sub>1</sub>* and put the result in register *D*. |
| `*` | *S<sub>1</sub>* | *S<sub>2</sub>* | *D* | **Multiply** the values from registers *S<sub>1</sub>* and *S<sub>2</sub>* and put the result in register *D*. |
| `/` | *S<sub>1</sub>* | *S<sub>2</sub>* | *D* | **Divide** the value of registers *S<sub>2</sub>* from *S<sub>1</sub>* and put the result in register *D*. |
| `%` | *S<sub>1</sub>* | *S<sub>2</sub>* | *D* | Divide the value of registers *S<sub>2</sub>* from *S<sub>1</sub>* and put the **remainder** in register *D*. |
|     |     |     |   |   |
| `B` | *S* | *H_h* | *H_l* | **Branch**.  Add *H_hH_l* times *2* to the PC if the value in register *S* is *not* zero. |
| `b` | *S* | *H_h* | *H_l* | **Branch**.  Subtract *H_hH_l* times *2* from the PC if the value in register *S* is *not* zero. |
| `E` | *S* | *H_h* | *H_l* | **Branch**.  Add *H_hH_l* times *2* to the PC if the value in register *S* is *equal* zero. |
| `e` | *S* | *H_h* | *H_l* | **Branch**.  Subtract *H_hH_l* times *2* from the PC if the value in register *S* is *equal* zero. |
| `<` | *S* | *H_h* | *H_l* | **Branch**.  Add *H_hH_l* times *2* to the PC if the value in register *S* is *less than* zero. |
| `l` | *S* | *H_h* | *H_l* | **Branch**.  Subtract *H_hH_l* times *2* from the PC if the value in register *S* is *less than* zero. |
| `>` | *S* | *H_h* | *H_l* | **Branch**.  Add *H_hH_l* times *2* to the PC if the value in register *S* is *greater than* zero. |
| `g` | *S* | *H_h* | *H_l* | **Branch**.  Subtract *H_hH_l* times *2* from the PC if the value in register *S* is *greater than* zero. |
|     |     |     |   |   |
| `R` | *S* | _ | _ | **Return**.  Set PC to the value in register *S*. |
| `H` | _ | _ | _ | **Halt**.  Stop executing. |


There are two additional instructions that take up 8 bytes each:

| Op  |     |     |     |     |     |     |     | Description |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `J` | *D* | _ | _ | *H<sub>3</sub>* | *H<sub>2</sub>* | *H<sub>1</sub>* | *H<sub>0</sub>* | **Jump**. Set register *D* to the value of PC plus 4.  Set PC to the value given by the hex string *H<sub>3</sub>H<sub>2</sub>H<sub>1</sub>H<sub>0</sub>*. |
| `I` | *D* | _ | _ | *H<sub>3</sub>* | *H<sub>2</sub>* | *H<sub>1</sub>* | *H<sub>0</sub>* | Load **Immediate**. Set register *D* to the value given by the hex string *H<sub>3</sub>H<sub>2</sub>H<sub>1</sub>H<sub>0</sub>*.|

The machine itself is 16-bit, each register holds a 16-bit value.
Each instruction takes up either two or four memory locations. The
registers named by the ASCII digit characters '`0`'
through '`9`' hold the value of that digit and cannot
be overwritten.  All other registers initially are zero.  values
for conditions should be interpreted as two's complement 16-bit
integers.  The program counter (PC) is register '`P`'
(initially zero, so all programs start executing with the instruction
at memory location zero).  Unless otherwise noted, the PC will increase
by two after each instruction.

## Operating System Support

R-ASCII ISA supports operating system interaction with a "syscall" instruction:

| Op |   |   |   | Description |
| --- | --- | --- | --- | --- |
| `!` | *M* | *H_h* | *H_l* | **Syscall**.  Invoke operating system functionality number *H_hH_l* (in hexadecimal) reading from and/or writing to the register given as the second character. |

We will only have very simple I/O operations:

| Syscall | Description |
| --- | --- |
| `!a01` | Print string.  OS will output a NULL terminated UTF-16 string from memory at the address given by register `a`. (Note: none of the examples use values outside of the printable ASCII so you can treat the values as ASCII bytes if that is easier). |
| `!a02` | Read word.  OS will read input from the user, parse it as a 16-bit number, and write that value to register `a`. |

## R-ASCII Executable File Format

Executable programs are written in an easy to parse file format.  The file
is divided up into <i>code</i> and <i>data</i> sections signaled by a
header line.  For example:

    code: 0x1000

indicates the start of a code section with instructions to be placed in
memory location 1000 (hexadecimal).  Inside code sections each line
will be four bytes of instruction.  Eight byte instructions will span
two lines.  The first four non-whitespace ASCII bytes of each of these
lines will fill two memory locations.

Data sections have a similar header:

    data: 0x2000 10

Like with code sections the first number in the header is the
memory location for subsequent data values.  The next numbers on
the header line specify how to visualize the data and do not
affect execution and may be omitted.  In this example, a single
number indicates that this data section should be visualized with
ten memory entries.  Values in data sections are given by
whitespace separated decimal (no prefix) or hexadecimal
('<code>0x</code>' prefix) numbers. These can span several lines.
Data sections can also be empty.

The '<code>#</code>' character at the beginning of a line is a comment and the line
can be ignored.  Comments can also be put at the end of lines (all characters after
and including '<code>#</code>' can be ignored).
The beginning of the file is assumed to be in the code section at memory
location zero.  That is, there is an implicit code header before the
first line of the file:

    code: 0x0
    
## Example

For example, the following program will write to the output
memory (address starting at <code>0x2000</code>) the values
from the input (0x1000) with the ones at the beginning
and zeros at the end.

````
Ia
1000
Ib
2000
Lan
+a1a # loop:
Lax
Ex03
Sxb
+b1b
-n1n
bn06 # <- loop
H

data: 0x1000 9  # input
8 0 1 1 0 1 1 0 0 

data: 0x2000 8  # output
````