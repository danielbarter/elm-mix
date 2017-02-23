module ASMParser exposing (..)

import StateMonad exposing (..)
import Assembler exposing (..)
import Instructions exposing (..)

import Regex


type TokenError = NoMatch
                | StringEmpty

type alias Tokenizer a = State String TokenError a
                  
getLexeme : Regex.Regex -> Tokenizer String

getLexeme r s =
    if String.isEmpty s
    then Err StringEmpty
    else case Regex.find (Regex.AtMost 1) r s of
             []      -> Err NoMatch
             (m::ms) -> let lexeme = .match m
                            index = .index m
                        in if index == 0
                           then Ok (lexeme,
                                     String.dropLeft (String.length lexeme) s)
                           else Err NoMatch
