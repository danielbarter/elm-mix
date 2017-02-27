{-

This codes parses a sequence of tokens into a list of REL expressions from CodeFunctor

(label :) (maks) inst a (i)

-}

module Parser exposing (parse)

import Atom exposing (..)
import StateMonad exposing (..)
import Tokenizer exposing (..)
import CodeFunctor exposing (..)
import Instruction exposing (..)

type ParserError = EndOfStream
                 | UnexpectedToken

type alias Parser a = State (List Token) ParserError a


getToken : Parser Token
getToken =
    let f l =
            case l of
                [] -> throwError EndOfStream
                (t::ts) -> return t
    in get >>= f

consumeToken : Parser ()
consumeToken =
    let f l =
            case l of
                [] -> throwError EndOfStream
                (t::ts) -> put ts
    in get >>= f

grabLabel : Parser (Maybe String)
grabLabel =
    let f t =
            case t of
                Lab name -> (return <| Just name) <* consumeToken <* consumeToken
                _        -> (return Nothing)
    in getToken >>= f

grabMask : Parser (Maybe Masks)
grabMask =
    let f t =
            case t of
                N n -> (return <| Just <| byteToMasks <| byte n) <* consumeToken
                _   -> (return Nothing)
    in getToken >>= f

grabTag : Parser Tag
grabTag =
   let f t =
           case t of
               I inst -> (return inst) <* consumeToken
               _      -> throwError UnexpectedToken
   in getToken >>= f

grabRelativeAddress : Parser RelativeAddress
grabRelativeAddress =
    let f t =
            case t of
                N n -> (return <| Value n) <* consumeToken
                Lab name -> (return <| Label name ) <* consumeToken
                _ -> throwError UnexpectedToken
    in getToken >>= f

grabInt : Parser (Maybe Int)
grabInt =
    let f t =
            case t of
                N n -> (return <| Just n) <* consumeToken
                _   -> (return Nothing)
    in getToken >>= f

grabIndex = try grabInt (return Nothing)

f l ms t a i =
    Code l ( DataInstruction
                 ( a
                 , Maybe.withDefault 0 i
                 , Maybe.withDefault (byteToMasks zero) ms
                 , t
                 )
           )
grabInstruction : Parser REL
grabInstruction = map5 f
                  grabLabel
                  grabMask
                  grabTag
                  grabRelativeAddress
                  grabInt

g l v = Code l (DataNumber <| Maybe.withDefault 0 v)

grabLiteral = map2 g grabLabel grabInt

parseLine : List Token -> Result ParserError REL
parseLine stream = Result.map Tuple.second
                   <| (try grabInstruction grabLiteral) stream

parse : List (List Token) -> Result ParserError (List REL)
parse s = distrubuteError <| List.map parseLine s
