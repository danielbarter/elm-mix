# MIX1010

In this document, we explain the **MIX1010 machine**. It is a slightly modified version of Knuth's MIX1009 machine which is used in [The Art of Computer Programming](https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming).

### Knuth Bytes

Since the MIX1009 predates modern computing, it encodes data in a somewhat strange way. The simplest unit of data in the machine is a **Knuth byte** which can represent 100 distinct values. In the MIX1010, a Knuth byte is represented using a 2 digit base 10 number: 00,01,02,...,99. A **small Knuth word** consists of two Knuth bytes and a sign: `s b1 b2`. A **Knuth word** consists of five Knuth bytes and a sign: `s b1 b2 b3 b4 b5`. In the MIX1010 machine, small Knuth words will be represented as signed 4 digit base 10 numbers. Knuth words will be represented as signed 10 digit base 10 numbers. From now on, we will drop prefix Knuth when talking about Knuth bytes, small Knuth words and Knuth words.

### Registers

The MIX1010 has a series of registers. The **A** and **X** registers each hold a word. The registers **I1,I2,I3,I4,I5,I6** and **J** each hold a small word. There is also an overflow register which takes the values `{Good,Overflow}` and a comparison register with takes the values `{>,=,<}`. 

### Memory

The MIX1010 has a memory bank indexed by positive small words. Each memory location contains a word.

### Instruction Encoding

The MIX1010 machine is a [Von Neumann](https://en.wikipedia.org/wiki/Von_Neumann_architecture), that is, programs are stored in memory. We encode instructions in a word as follows: `s a1 a2 i f c`. The small word `s a1 a2` is called the **static address**. The bytes `i,f,c` are called the **index**, **modification** and **code** respectively.

### Dynamic Addresses 

The **dynamic address** of an encoded instruction is the static address plus the value in the register `Ii` where `i` is the index byte. Indeed, if the index byte is greater than 6, then a runtime error will occur. Many instructions use the address field differently, but they always use the dynamic address instead of the static address.

### Masking

Some instructions use the modification byte for masking. This works as follows: Take the lowest 6 digits in the binary expansion for `f`. The digit `1` corresponds to a mask. Here is an example:
```
\5 LDA x
:x 9999999999
```
The first instruction will load 9999009900 into register `A`. This is because 5 = 000101, so we mask of the bytes 3 and 5.

### Assembly 

The assembly language used by the machine is slightly different from the MIX 1009. Statements are of the form
```
(:label) (/mask) (instruction) (relative address) (+index)
```
Like most assembly languages, we can refer to memory locations using labels. These get resolved at compile time by the assembler. The mask is indicated by a forward slash `/`. If the instruction doesn't use a mask, then it will be ignored. The relative address can be a label or a explicit memory location. The assembler will not be happy if you try to use a label which has not been declared. The only way to force the assembler to put a block of instructions at a specific location is to pad the code with zeros, which is the default memory location. Empty lines are ignored. Comments are prefixed by `#`. In memory, instructions are represented in the form
```
address(:label) (/mask) (instruction) (address) (+index)
```

### Instructions

The MIX 1010 has a lot of instructions:

- `LDA,LDX,LDi`. Load a memory value into the corresponding register.
- `LDAN,LDXN,LDiN`. Load the negative of a memory value into the corresponding register.
- `STA,STX,STi`. Store the corresponding register into memory.
- `STJ`. Store the jump register in memory.
- `STZ`. Store zero in memory.
- `ADD`. Add memory value to register `A`.
- `SUB`. subtract memory value from register `A`.
- `ADDX`. Add memory value to register `X`.
- `SUBX`. subtract memory value from register `X`.
- `ENTA,ENTX,ENTi`. Enter value into corresponding register.
- `ENNA,ENNX,ENNi`. Enter negative of value into corresponding register.
- `INCA,INCX,INCi`. Increment corresponding register by value.
- `DECA,DECX,DECi`. Decrement corresponding register by value.
- `CMPA,CMPX,CMPi`. Compare corresponding register to a memory value.
- `JMP`. Jump to location. `J` is set to the address of the instruction that would have been executed next if we hadn't jumped. This is the default behaviour for jump instructions, and we don't say it again unless something different happens.
- `JSJ`. Jump to location but don't update `J`.
- `JOV`. Jump to location on overflow.
- `JNOV`. Jump to location on no overflow.
- `JL,JE,JG,JGE,JNE,JLE`. Jump on less,equal,greater,greater-or-equal,unequal,less-or-equal.
- `JAN,JAZ,JAP,JANN,JANZ,JANP` , `JXN,JXZ,JXP,JXNN,JXNZ,JXNP` , `JiN,JiZ,JiP,JiNN,JiNZ,JiNP`. Jump if corresponding register is negative,zero,positive,nonnegative,nonzero,nonpositive.
- `SA`. Shift `A` by value.
- `SX`. Shift `X` by value.
- `SAC`. Shift `A` cyclically by value.
- `SAX`. Shift `X` cyclically by value.
- `SWAP`. Swap the `X` and `A` registers.
- `MOVXi`. Move the value in `X` to `Ii`.
- `NOP`. No instruction.
- `HLT`. Halt.
