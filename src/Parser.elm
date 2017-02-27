{-

This codes parses a sequence of tokens into a list of REL expressions from CodeFunctor

(label :) (maks) inst (a) (i)
(label :) a
-}

module Parser exposing (..)


import Atom exposing (..)
import StateMonad exposing (..)
import Tokenizer exposing (..)
import CodeFunctor exposing (..)
import Instruction exposing (..)

type ParserError = EndOfStream
                 | UnexpectedToken

type alias Parser a = State (List Token) ParserError a



{-
parseLine : List Token -> Result ParserError REL

parse : List (List Token) -> Result ParserError (List REL)
parse s = distrubuteError <| List.map parseLine s
-}
