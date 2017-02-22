module ASM exposing (..)

import Mix exposing ( Address
                    , Index
                    , Modification
                    , InstructionCode
                    , Memory
                    , read
                    , Mix
                    , RuntimeError(..)
                    , MixOperation
                    , Instruction(..)
                    , step
                    , instruction
                    )

import Atom exposing ( Base
                     , Sign(..)
                     , swap
                     , baseExpand
                     , baseExpandPad
                     , baseContract
                     , mixBase
                     , Byte
                     , byte
                     , zero
                     , zeroWord
                     , zeroSmallWord
                     , value
                     , SmallWord
                     , Word
                     , wordExpand
                     , wordContract
                     , Mask(..)
                     , Masks
                     , maskFilter
                     , copy
                     , wordValue
                     , smallWordValue
                     , byteToMasks
                     , masksToByte
                     , flipSignWord
                     , flipSignSmallWord
                     , OverflowToggle(..)
                     , ComparisonIndicator(..)
                     , op
                     , intToWord
                     , intToSmallWord
                     , comp
                     , shift
                     , shiftCircular
                     )


type Instruction = LDA Address Index Mask
                 | LDX Address Index Mask
                 | LD1 Address Index Mask
                 | LD2 Address Index Mask
                 | LD3 Address Index Mask
                 | LD4 Address Index Mask
                 | LD5 Address Index Mask
                 | LD6 Address Index Mask
                 | LDAN Address Index Mask
                 | LDXN Address Index Mask
                 | LD1N Address Index Mask
                 | LD2N Address Index Mask
                 | LD3N Address Index Mask
                 | LD4N Address Index Mask
                 | LD5N Address Index Mask
                 | LD6N Address Index Mask
                 | STA Address Index Mask
                 | STX Address Index Mask
                 | ST1 Address Index Mask
                 | ST2 Address Index Mask     
                 | ST3 Address Index Mask
                 | ST4 Address Index Mask
                 | ST5 Address Index Mask
                 | ST6 Address Index Mask
                 | STJ Address Index Mask
                 | STZ Address Index Mask
                 | ADD Address Index Mask
                 | SUB Address Index Mask
                 | ADDX Address Index Mask
                 | SUBX Address Index Mask
                 | ENTA Address Index
                 | ENTX Address Index
                 | ENT1 Address Index
                 | ENT2 Address Index
                 | ENT3 Address Index
                 | ENT4 Address Index
                 | ENT5 Address Index
                 | ENT6 Address Index
                 | ENNA Address Index
                 | ENNX Address Index
                 | ENN1 Address Index
                 | ENN2 Address Index
                 | ENN3 Address Index
                 | ENN4 Address Index
                 | ENN5 Address Index
                 | ENN6 Address Index
                 | INCA Address Index
                 | INCX Address Index
                 | INC1 Address Index
                 | INC2 Address Index
                 | INC3 Address Index
                 | INC4 Address Index
                 | INC5 Address Index
                 | INC6 Address Index
                 | DECA Address Index
                 | DECX Address Index
                 | DEC1 Address Index
                 | DEC2 Address Index
                 | DEC3 Address Index
                 | DEC4 Address Index
                 | DEC5 Address Index
                 | DEC6 Address Index
                 | CMPA Address Index Mask
                 | CMPX Address Index Mask
                 | CMP1 Address Index Mask
                 | CMP2 Address Index Mask
                 | CMP3 Address Index Mask
                 | CMP4 Address Index Mask
                 | CMP5 Address Index Mask
                 | CMP6 Address Index Mask
                 | JMP Address Index
                 | JSJ Address Index
                 | JOV Address Index
                 | JNOV Address Index
                 | JL Address Index
                 | JE Address Index
                 | JG Address Index
                 | JGE Address Index
                 | JNE Address Index
                 | JLE Address Index
                 | JAN Address Index
                 | JAZ Address Index
                 | JAP Address Index
                 | JANN Address Index
                 | JANZ Address Index
                 | JANP Address Index
                 | JXN Address Index
                 | JXZ Address Index
                 | JXP Address Index
                 | JXNN Address Index
                 | JXNZ Address Index
                 | JXNP Address Index
                 | J1N Address Index
                 | J1Z Address Index
                 | J1P Address Index
                 | J1NN Address Index
                 | J1NZ Address Index
                 | J1NP Address Index
                 | J2N Address Index
                 | J2Z Address Index
                 | J2P Address Index
                 | J2NN Address Index
                 | J2NZ Address Index
                 | J2NP Address Index
                 | J3N Address Index
                 | J3Z Address Index
                 | J3P Address Index
                 | J3NN Address Index
                 | J3NZ Address Index
                 | J3NP Address Index
                 | J4N Address Index
                 | J4Z Address Index
                 | J4P Address Index
                 | J4NN Address Index
                 | J4NZ Address Index
                 | J4NP Address Index
                 | J5N Address Index
                 | J5Z Address Index
                 | J5P Address Index
                 | J5NN Address Index
                 | J5NZ Address Index
                 | J5NP Address Index
                 | J6N Address Index
                 | J6Z Address Index
                 | J6P Address Index
                 | J6NN Address Index
                 | J6NZ Address Index
                 | J6NP Address Index
                 | SA Address Index
                 | SX Address Index
                 | SAC Address Index
                 | SAX Address Index
                 | SWAP
                 | MOVX1
                 | MOVX2
                 | MOVX3
                 | MOVX4
                 | MOVX5
                 | MOVX6
                 | NOP
                 | HLT
