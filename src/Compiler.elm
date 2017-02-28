{-

The compiler combines the tokenizer,parser,transpiler and assembler to produce a memory image from source code

-}

module Compiler exposing ( compile
                         , CompilerError
                         )

import Tokenizer exposing (..)
import Parser exposing (..)
import Transpiler exposing (..)
import Assembler exposing (..)
import Mix exposing (..)
import Dict

type CompilerError = ParserPhaseError ParserError
                   | TranspilePhaseError TranspileError

compile : String
        -> Result CompilerError ( Memory
                                , MetaMemory
                                , SymbolTable
                                , ReverseSymbolTable
                                )

compile s =
    let t = tokenize s
        p = parse t
    in case p of
           Err err -> Err <| ParserPhaseError err
           Ok r -> case transpile r of
                       Err err -> Err <| TranspilePhaseError err
                       Ok (a,st) -> let (meta,mem) = assemble a
                               in Ok (mem,meta,st,reverse st)


reverse : Dict.Dict String Int -> Dict.Dict Int String
reverse d = Dict.fromList <| List.map (\(x,y) -> (y,x)) <| Dict.toList d

