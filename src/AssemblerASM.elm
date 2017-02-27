module AssemblerASM exposing ( LineASM
                             , assembleLine
                             , assembleASM
                             )


import Atom exposing (..)
import Instruction exposing (..)
import StateMonad exposing (..)
import Mix exposing (..)
import Tokenizer exposing (..)
import LineFunctor exposing (..)
import Dict

type alias LineASM = LineFunctor Address StaticInstruction


assembleLine : LineASM -> ((Address,Word),(Address,MetaData))
assembleLine (LineFunctor a i) =
    case i of
        DataNumber n -> ((a,Tuple.second <| intToWord n zeroWord)
                       ,(a,Number)
                       )
        DataInstruction x -> ((a, pack <| encodeInstruction x)
                            ,(a,Instruction)
                            )

assembleASM : List LineASM -> (MetaMemory,Memory)
assembleASM l =
    let meta = Dict.fromList <| List.map (Tuple.second << assembleLine) l
        mem = Dict.fromList <| List.map (Tuple.first << assembleLine) l
    in (meta, mem)



