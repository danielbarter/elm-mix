{-

This codes parses a sequence of tokens into a list of REL expressions from CodeFunctor

(:label) (/mask) instruction (relative address or [relative addresses]) (+index)
-}

module Parser exposing ( parse
                       , ParserError(..)
                       )


import Atom exposing (..)
import Tokenizer exposing (..)
import CodeFunctor exposing (..)
import Instruction exposing (..)
import StateMonad exposing (..)


type ParserError = EndOfStream
                 | UnexpectedToken Token


type alias Parser a = State (List Token) ParserError a

token : List Token -> Parser Token
token l =
    case l of
        [] -> throwError EndOfStream
        t::ts -> (return t) <* put ts

endOfStream : List Token -> Parser ()
endOfStream l =
    case l of
        []    -> return ()
        t::ts -> throwError <| UnexpectedToken t

label : Token -> Parser String
label t =
    case t of
        LabelTag n -> return n
        _          -> throwError <| UnexpectedToken t

mask : Token -> Parser Masks
mask t =
    case t of
        MaskTag m -> return <| ( byteToMasks << byte ) m
        _         -> throwError <| UnexpectedToken t

instruction : Token -> Parser Tag
instruction t =
    case t of
        InstructionTag i -> return i
        _                -> throwError <| UnexpectedToken t

relativeAddress : Token -> Parser RelativeAddress
relativeAddress t =
    case t of
        RelativeTag a -> return a
        _             -> throwError <| UnexpectedToken t

index : Token -> Parser Int
index t =
    case t of
        IndexTag i -> return i
        _          -> throwError <| UnexpectedToken t

comment : Token -> Parser String
comment t =
    case t of
        Comment s -> return s
        _         -> throwError <| UnexpectedToken t

schema : Token -> Parser Schema
schema t =
    case t of
        SchemaTag s -> return s
        _           -> throwError <| UnexpectedToken t


leftBracket : Token -> Parser ()
leftBracket t =
    case t of
        LeftBracket -> return ()
        _           -> throwError <| UnexpectedToken t

rightBracket : Token -> Parser ()
rightBracket t =
    case t of
        RightBracket -> return ()
        _            -> throwError <| UnexpectedToken t


p l m t a i = Just <| Code l <| DataInstruction (a,i,m,t)

parseInstruction : Parser (Maybe REL)
parseInstruction =
    (map5 p
        (try (Just <$> (get >>= token >>= label)) (return Nothing))
        (try (get >>= token >>= mask) (return <| (byteToMasks << byte) 0))
        (get >>= token >>= instruction)
        (try (get >>= token >>= relativeAddress) (return <| Value 0))
        (try (get >>= token >>= index) (return 0))
    ) <* (try (get >>= token >>= comment *> return ()) (return ()))
      <* (get >>= endOfStream)


q l a = Just <| Code l <| DataNumber <| DataAddress a

parseWord : Parser (Maybe REL)
parseWord =
    (map2 q
        (try (Just <$> (get >>= token >>= label)) (return Nothing))
        (try (get >>= token >>= relativeAddress) (return <| Value 0))
    ) <* (try (get >>= token >>= comment *> return ()) (return ()))
      <* (get >>= endOfStream)

r l s a = Just <| Code l <| DataNumber <| DataPacked s a

parseSchema : Parser (Maybe REL)
parseSchema =
    (map3 r
        (try (Just <$> (get >>= token >>= label)) (return Nothing))
        (get >>= token >>= schema)
        ( (get >>= token >>= leftBracket) *>
          (repeat (get >>= token >>= relativeAddress)) <*
          (get >>= token >>= rightBracket))
    ) <* (try (get >>= token >>= comment *> return ()) (return ()))
      <* (get >>= endOfStream)

parseComment = (get >>= token >>= comment) *>
               (get >>= endOfStream) *>
               (return Nothing)

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



