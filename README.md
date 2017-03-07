# Introduction

In this document, we explain the MIX1010 machine. It is a slightly modified version of Knuth's MIX1009 machine which is used in [The Art of Computer Programming](https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming).

# Knuth Bytes

Since the MIX1009 predates modern computing, it encodes data in a somewhat strange way. The simplest unit of data in the machine is a *Knuth byte* which can represent 100 distinct values. In the MIX1010, a Knuth byte is represented using a 2 digit base 10 number: 00,01,02,...,99. A *small Knuth word* consists of two Knuth bytes and a sign: `s b1 b2`. A *Knuth word* consists of five Knuth bytes and a sign: `s b1 b2 b3 b4 b5`. In the MIX1010 machine, small Knuth words will be represented as signed 4 digit base 10 numbers. Knuth words will be represented as signed 10 digit base 10 numbers. From now on, we will drop prefix Knuth when talking about Knuth bytes, small Knuth words and Knuth words.

# Registers

The MIX1010 has a series of registers. The *A* and *X* registers each hold a word. The registers *I1,I2,I3,I4,I5,I6* and *J* each hold a small word. There is also an overflow register which takes the values `Good,Overflow` and a comparison register with takes the values `>,=,<`. 
