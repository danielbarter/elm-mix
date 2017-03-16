{-

the tokenizer converts a string into a list of tokens for parsing.

(:label) (/mask) instruction (relative address) (+index)

-}

module Tokenizer exposing ( tokenize
                          , Token(..)
                          , filterNothing
                          , distrubuteError
                          )


import StateMonad exposing (..)
import Instruction exposing (..)
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
                           then Ok ( String.dropLeft (String.length lexeme) s
                                   , lexeme
                                   )
                           else Err NoMatch

type Token = InstructionTag Tag
           | RelativeTag RelativeAddress
           | LabelTag String
           | MaskTag Int
           | IndexTag Int
           | Comment String


getToken = List.foldl try getInstruction
           [ getRelative
           , getLabel
           , getMask
           , getIndex
           , getComment
           ]

getRelValue = ((RelativeTag << Value) << (Result.withDefault 0) << String.toInt)
              <$> (getLexeme <| Regex.regex "-?[0-9]+")
getRelLabel = (RelativeTag << Label)
              <$> (getLexeme <| Regex.regex "[a-z]([a-z]|_)*")
getRelative = try getRelValue getRelLabel


getLabel =  (LabelTag << (String.dropLeft 1) )
            <$> (getLexeme <| Regex.regex "[:][a-z]([a-z]|_)*")

getMask = ( MaskTag
                << (Result.withDefault 0)
                << String.toInt
                << (String.dropLeft 1) )
          <$> (getLexeme <| Regex.regex "[/][0-9]+")

getIndex = ( IndexTag
                << (Result.withDefault 0)
                << String.toInt
                << (String.dropLeft 1) )
          <$> (getLexeme <| Regex.regex "[+][0-9]+")

getComment = (Comment << (String.dropLeft 1))
             <$> (getLexeme <| Regex.regex "#(.)*")

f t r = (return <| InstructionTag <| t) <* (getLexeme <| Regex.regex r)

getInstruction = List.foldl try (f  Halt "HLT")
                 [ f   LoadA              "LDA"
                 , f   LoadX              "LDX"
                 , f   LoadI1             "LD1"
                 , f   LoadI2             "LD2"
                 , f   LoadI3             "LD3"
                 , f   LoadI4             "LD4"
                 , f   LoadI5             "LD5"
                 , f   LoadI6             "LD6"
                 , f   LoadANeg           "LDAN"
                 , f   LoadXNeg           "LDXN"
                 , f   LoadI1Neg          "LD1N"
                 , f   LoadI2Neg          "LD2N"
                 , f   LoadI3Neg          "LD3N"
                 , f   LoadI4Neg          "LD4N"
                 , f   LoadI5Neg          "LD5N"
                 , f   LoadI6Neg          "LD6N"
                 , f   StoreA             "STA"
                 , f   StoreX             "STX"
                 , f   StoreI1            "ST1"
                 , f   StoreI2            "ST2"
                 , f   StoreI3            "ST3"
                 , f   StoreI4            "ST4"
                 , f   StoreI5            "ST5"
                 , f   StoreI6            "ST6"
                 , f   StoreJ             "STJ"
                 , f   StoreZero          "STZ"
                 , f   Add                "ADD"
                 , f   Sub                "SUB"
                 , f   AddX               "ADDX"
                 , f   SubX               "SUBX"
                 , f   EnterA             "ENTA"
                 , f   EnterX             "ENTX"
                 , f   EnterI1            "ENT1"
                 , f   EnterI2            "ENT2"
                 , f   EnterI3            "ENT3"
                 , f   EnterI4            "ENT4"
                 , f   EnterI5            "ENT5"
                 , f   EnterI6            "ENT6"
                 , f   EnterANeg          "ENNA"
                 , f   EnterXNeg          "ENNX"
                 , f   EnterI1Neg         "ENN1"
                 , f   EnterI2Neg         "ENN2"
                 , f   EnterI3Neg         "ENN3"
                 , f   EnterI4Neg         "ENN4"
                 , f   EnterI5Neg         "ENN5"
                 , f   EnterI6Neg         "ENN6"
                 , f   IncrementA         "INCA"
                 , f   IncrementX         "INCX"
                 , f   IncrementI1        "INC1"
                 , f   IncrementI2        "INC2"
                 , f   IncrementI3        "INC3"
                 , f   IncrementI4        "INC4"
                 , f   IncrementI5        "INC5"
                 , f   IncrementI6        "INC6"
                 , f   DecrementA         "DECA"
                 , f   DecrementX         "DECX"
                 , f   DecrementI1        "DEC1"
                 , f   DecrementI2        "DEC2"
                 , f   DecrementI3        "DEC3"
                 , f   DecrementI4        "DEC4"
                 , f   DecrementI5        "DEC5"
                 , f   DecrementI6        "DEC6"
                 , f   CompareA           "CMPA"
                 , f   CompareX           "CMPX"
                 , f   CompareI1          "CMP1"
                 , f   CompareI2          "CMP2"
                 , f   CompareI3          "CMP3"
                 , f   CompareI4          "CMP4"
                 , f   CompareI5          "CMP5"
                 , f   CompareI6          "CMP6"
                 , f   Jump               "JMP"
                 , f   JumpSaveJ          "JSJ"
                 , f   JumpOnOverflow     "JOV"
                 , f   JumpOnNoOverflow   "JNOV"
                 , f   JumpOnLess         "JL"
                 , f   JumpOnEqual        "JE"
                 , f   JumpOnGreater      "JG"
                 , f   JumpOnGreaterEqual "JGE"
                 , f   JumpOnUnEqual      "JNE"
                 , f   JumpOnLessEqual    "JLE"
                 , f   JumpANegative      "JAN"
                 , f   JumpAZero          "JAZ"
                 , f   JumpAPositive      "JAP"
                 , f   JumpANonNegative   "JANN"
                 , f   JumpANonZero       "JANZ"
                 , f   JumpANonPositive   "JANP"
                 , f   JumpXNegative      "JXN"
                 , f   JumpXZero          "JXZ"
                 , f   JumpXPositive      "JXP"
                 , f   JumpXNonNegative   "JXNN"
                 , f   JumpXNonZero       "JXNZ"
                 , f   JumpXNonPositive   "JXNP"
                 , f   JumpI1Negative     "J1N"
                 , f   JumpI1Zero         "J1Z"
                 , f   JumpI1Positive     "J1P"
                 , f   JumpI1NonNegative  "J1NN"
                 , f   JumpI1NonZero      "J1NZ"
                 , f   JumpI1NonPositive  "J1NP"
                 , f   JumpI2Negative     "J2N"
                 , f   JumpI2Zero         "J2Z"
                 , f   JumpI2Positive     "J2P"
                 , f   JumpI2NonNegative  "J2NN"
                 , f   JumpI2NonZero      "J2NZ"
                 , f   JumpI2NonPositive  "J2NP"
                 , f   JumpI3Negative     "J3N"
                 , f   JumpI3Zero         "J3Z"
                 , f   JumpI3Positive     "J3P"
                 , f   JumpI3NonNegative  "J3NN"
                 , f   JumpI3NonZero      "J3NZ"
                 , f   JumpI3NonPositive  "J3NP"
                 , f   JumpI4Negative     "J4N"
                 , f   JumpI4Zero         "J4Z"
                 , f   JumpI4Positive     "J4P"
                 , f   JumpI4NonNegative  "J4NN"
                 , f   JumpI4NonZero      "J4NZ"
                 , f   JumpI4NonPositive  "J4NP"
                 , f   JumpI5Negative     "J5N"
                 , f   JumpI5Zero         "J5Z"
                 , f   JumpI5Positive     "J5P"
                 , f   JumpI5NonNegative  "J5NN"
                 , f   JumpI5NonZero      "J5NZ"
                 , f   JumpI5NonPositive  "J5NP"
                 , f   JumpI6Negative     "J6N"
                 , f   JumpI6Zero         "J6Z"
                 , f   JumpI6Positive     "J6P"
                 , f   JumpI6NonNegative  "J6NN"
                 , f   JumpI6NonZero      "J6NZ"
                 , f   JumpI6NonPositive  "J6NP"
                 , f   ShiftA             "SA"
                 , f   ShiftX             "SX"
                 , f   ShiftACircular     "SAC"
                 , f   ShiftXCircular     "SAX"
                 , f  SwapAX              "SWAP"
                 , f  MoveXI1             "MOVX1"
                 , f  MoveXI2             "MOVX2"
                 , f  MoveXI3             "MOVX3"
                 , f  MoveXI4             "MOVX4"
                 , f  MoveXI5             "MOVX5"
                 , f  MoveXI6             "MOVX6"
                 , f  MoveI1X             "MOV1X"
                 , f  MoveI2X             "MOV2X"
                 , f  MoveI3X             "MOV3X"
                 , f  MoveI4X             "MOV4X"
                 , f  MoveI5X             "MOV5X"
                 , f  MoveI6X             "MOV6X"
                 , f  MoveJX              "MOVJX"
                 , f  NoOperation         "NOP"
                 ]



tokenizeLine : String -> List Token
tokenizeLine s = case getToken s of
                     Ok (s1,t)   -> t :: (tokenizeLine s1)
                     Err NoMatch -> case String.uncons s of
                                        Nothing -> []
                                        Just (c,s2) -> tokenizeLine s2
                     Err StringEmpty -> []

tagEmptyLine : List a -> Maybe (List a)
tagEmptyLine l =
    case l of
        [] -> Nothing
        _  -> Just l

filterNothing : List (Maybe a) -> List a
filterNothing l =
    case l of
        [] -> []
        (x::xs) -> case x of
                       Just z -> z :: (filterNothing xs)
                       Nothing -> filterNothing xs


distrubuteError : List (Result e a) -> Result e (List a)
distrubuteError l =
    case l of
        [] -> Ok []
        (x::xs) -> case x of
                       Err err -> Err err
                       Ok t -> Result.map ((::) t) <| distrubuteError xs


commentLine : List Token -> Bool
commentLine l =
    case l of
        (Comment s)::ts -> False
        _ -> True

tokenize : String -> List (List Token)
tokenize s = List.filter commentLine <| filterNothing
                  <| List.map (tagEmptyLine << tokenizeLine)
                  <| String.lines s

