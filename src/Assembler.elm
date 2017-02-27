{-

The assembler translates a list of ASM code into a memory image

-}

module Assembler exposing (assemble)

import CodeFunctor exposing (..)
import Mix exposing (..)
import Instruction exposing (..)
import Atom exposing (..)
import Dict

assembleLine : ASM -> ((Address,Word),(Address,MetaData))
assembleLine (Code a i) =
    case i of
        DataNumber n -> ((a,Tuple.second <| intToWord n zeroWord)
                       ,(a,Number)
                       )
        DataInstruction x -> ((a, pack <| encodeInstruction x)
                            ,(a,Instruction)
                            )

assemble : List ASM -> (MetaMemory,Memory)
assemble l =
    let meta = Dict.fromList <| List.map (Tuple.second << assembleLine) l
        mem = Dict.fromList <| List.map (Tuple.first << assembleLine) l
    in (meta, mem)
