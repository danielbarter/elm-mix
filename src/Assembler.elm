module Assembler exposing ( assemble
                          , parse
                          , compile
                          , CompileError(..)
                          )


import Atom exposing (..)
import Instruction exposing (..)
import Mix exposing (..)
import Tokenizer exposing (..)
import Dict

type ASM = ASMNumber Int
         | ASMInstruction StaticInstruction

type alias Line = (Address,ASM)


distrubuteError : List (Result e a) -> Result e (List a)
distrubuteError l =
    case l of
        [] -> Ok []
        (x::xs) -> case x of
                       Err err -> Err err
                       Ok t -> Result.map ((::) t) <| distrubuteError xs

filterNothing : List (Maybe a) -> List a
filterNothing l =
    case l of
        [] -> []
        (x::xs) -> case x of
                       Just z -> z :: (filterNothing xs)
                       Nothing -> filterNothing xs

type CompileError = UnexpectedTokenSequence


parseLine : List Token -> Result CompileError (Maybe Line)
parseLine l =
    case l of
        [] -> Ok <| Nothing
        [N n,Colon,I t, N a]
            -> Ok <| Just (n,ASMInstruction (a,0,byteToMasks <| byte 0,t))
        [N n, Colon, I t, N a, Comma, N i]
            -> Ok <| Just (n, ASMInstruction (a,i,byteToMasks <| byte 0,t))
        [N n, Colon, N m, I t, N a, Comma, N i]
            -> Ok <| Just (n,ASMInstruction (a,i,byteToMasks <| byte m,t))
        [N n, Colon, N m, I t, N a]
            -> Ok <| Just (n,ASMInstruction (a,0,byteToMasks <| byte m,t))
        [N n, Colon, N m]
            -> Ok <| Just (n, ASMNumber m)
        _ -> Err UnexpectedTokenSequence



parse : List (List Token) -> Result CompileError (List Line)
parse s = Result.map filterNothing
            <| distrubuteError
            <| List.map parseLine s



assembleLine : Line -> ((Address,Word),(Address,MetaData))
assembleLine (a,i) =
    case i of
        ASMNumber n -> ((a,Tuple.second <| intToWord n zeroWord)
                       ,(a,Number)
                       )
        ASMInstruction x -> ((a, pack <| encodeInstruction x)
                            ,(a,Instruction)
                            )

assemble : List Line -> (MetaMemory,Memory)
assemble l =
    let meta = Dict.fromList <| List.map (Tuple.second << assembleLine) l
        mem = Dict.fromList <| List.map (Tuple.first << assembleLine) l
    in (meta, mem)


compile : String -> Result CompileError (MetaMemory,Memory)
compile s = Result.map assemble <| parse <| tokenizeLines s
