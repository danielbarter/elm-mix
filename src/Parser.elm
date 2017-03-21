{-

This codes parses a sequence of tokens into a list of REL expressions from CodeFunctor

(:label) (/mask) instruction (relative address or [relative addresses]) (+index)
-}

module Parser exposing (..)
{-
    ( parse
    , ParserError(..)
    )
-}

import Atom exposing (..)
import Tokenizer exposing (..)
import CodeFunctor exposing (..)
import Instruction exposing (..)
import StateMonad exposing (..)


type ParserError = EndOfStream
                 | UnexpectedToken Token


type alias Parser a = State (List Token) ParserError a


label : List Token -> Parser (Maybe String)
label l =
    case l of
        []    -> return Nothing
        t::ts -> case t of
                     LabelTag n -> (return <| Just n) <* put ts
                     _          -> return Nothing

getLabel = get >>= label

mask : List Token -> Parser (Maybe Int)
mask l =
    case l of
        []    -> return Nothing
        t::ts -> case t of
                     MaskTag m -> (return <| Just m) <* put ts
                     _         -> return Nothing

getMask = (byteToMasks << byte) <$> ((Maybe.withDefault 0) <$> (get >>= mask))


instruction : List Token -> Parser Tag
instruction l =
    case l of
        []    -> throwError EndOfStream
        t::ts -> case t of
                     InstructionTag i -> (return i) <* put ts
                     _ -> throwError <| UnexpectedToken t

getInstruction = get >>= instruction

relativeAddress : List Token -> Parser (Maybe RelativeAddress)
relativeAddress l =
    case l of
        []    -> return Nothing
        t::ts -> case t of
                     RelativeTag a -> (return <| Just a) <* put ts
                     _ -> return Nothing

getRelativeAddress =
    ((Maybe.withDefault <| Value 0) <$> (get >>= relativeAddress))


relativeAddressError : List Token -> Parser RelativeAddress
relativeAddressError l =
    case l of
        []    -> throwError EndOfStream
        t::ts -> case t of
                     RelativeTag a -> (return a) <* put ts
                     _ -> throwError <| UnexpectedToken t

getRelativeAddressError = get >>= relativeAddressError

index : List Token -> Parser (Maybe Int)
index l =
    case l of
        []    -> return Nothing
        t::ts -> case t of
                     IndexTag i -> (return <| Just i) <* put ts
                     _ -> return Nothing

comment : List Token -> Parser String
comment l =
    case l of
        []    -> throwError EndOfStream
        t::ts -> case t of
                     Comment s -> (return s) <* put ts
                     _         -> throwError <| UnexpectedToken t

getComment = get >>= comment




getIndex = (Maybe.withDefault 0) <$> (get >>= index)

schema : List Token -> Parser Schema
schema l =
    case l of
        []    -> throwError EndOfStream
        t::ts -> case t of
                     SchemaTag s -> (return s) <* put ts
                     _           -> throwError <| UnexpectedToken t

getSchema = get >>= schema

getSchemaArguments =
    (getLeftBracket *> (repeat getRelativeAddressError)) <* getRightBracket

leftBracket : List Token -> Parser ()
leftBracket l =
    case l of
        []    -> throwError EndOfStream
        t::ts -> case t of
                     LeftBracket -> put ts
                     _           -> throwError <| UnexpectedToken t

rightBracket : List Token -> Parser ()
rightBracket l =
    case l of
        []    -> throwError EndOfStream
        t::ts -> case t of
                     RightBracket -> put ts
                     _            -> throwError <| UnexpectedToken t


getLeftBracket = get >>= leftBracket
getRightBracket = get >>= rightBracket

p l m t a i = Just <| Code l <| DataInstruction (a,i,m,t)

parseInstruction : Parser (Maybe REL)
parseInstruction =
    map5 p
        getLabel
        getMask
        getInstruction
        getRelativeAddress
        getIndex

q l a = Just <| Code l <| DataNumber <| DataAddress a

parseWord : Parser (Maybe REL)
parseWord =
    map2 q
        getLabel
        getRelativeAddressError

r l s a = Just <| Code l <| DataNumber <| DataPacked s a

parseSchema : Parser (Maybe REL)
parseSchema =
    map3 r
        getLabel
        getSchema
        getSchemaArguments

parseComment = getComment *> (return Nothing)

parser : Parser (Maybe REL)
parser = List.foldl try parseInstruction
         [ parseWord
         , parseSchema
         , parseComment
         ]

parseLine : List Token -> Result ParserError (Maybe REL)
parseLine s = Result.map Tuple.second <| parser s



parse : List (List Token) -> Result ParserError (List REL)
parse s = Result.map filterNothing <| distrubuteError <| List.map parseLine s

