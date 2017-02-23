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
                           then Ok ( String.dropLeft (String.length lexeme) s
                                   , lexeme
                                   )
                           else Err NoMatch

type Token = I InstructionToken
           | N Int
           | Colon
           | Comma


f2 i r = (return <| I <| i () ()) <* (getLexeme <| Regex.regex r)
f1 i r = (return <| I <| i ()) <* (getLexeme <| Regex.regex r)
f0 i r = (return <| I <| i) <* (getLexeme <| Regex.regex r)

getInt = (N << (Result.withDefault 0) << String.toInt)
         <$> (getLexeme <| Regex.regex "-?[0-9]+")
         
getToken = List.foldl try (return Colon <* (getLexeme <| Regex.regex ":"))
           [ getInt
           , (return Comma <* (getLexeme <| Regex.regex ","))
           , f0 Halt "HLT"
           , f2  LoadA              "LDA"
           , f2  LoadX              "LDX"        
           , f2  LoadI1             "LD1"        
           , f2  LoadI2             "LD2"        
           , f2  LoadI3             "LD3"        
           , f2  LoadI4             "LD4"        
           , f2  LoadI5             "LD5"        
           , f2  LoadI6             "LD6"        
           , f2  LoadANeg           "LDAN"       
           , f2  LoadXNeg           "LDXN"       
           , f2  LoadI1Neg          "LD1N"       
           , f2  LoadI2Neg          "LD2N"       
           , f2  LoadI3Neg          "LD3N"       
           , f2  LoadI4Neg          "LD4N"       
           , f2  LoadI5Neg          "LD5N"       
           , f2  LoadI6Neg          "LD6N"       
           , f2  StoreA             "STA"        
           , f2  StoreX             "STX"        
           , f2  StoreI1            "ST1"        
           , f2  StoreI2            "ST2"        
           , f2  StoreI3            "ST3"        
           , f2  StoreI4            "ST4"        
           , f2  StoreI5            "ST5"        
           , f2  StoreI6            "ST6"        
           , f2  StoreJ             "STJ"        
           , f2  StoreZero          "STZ"        
           , f2  Add                "ADD"        
           , f2  Sub                "SUB"        
           , f1  AddX               "ADDX"       
           , f1  SubX               "SUBX"       
           , f1  EnterA             "ENTA"       
           , f1  EnterX             "ENTX"       
           , f1  EnterI1            "ENT1"       
           , f1  EnterI2            "ENT2"       
           , f1  EnterI3            "ENT3"       
           , f1  EnterI4            "ENT4"       
           , f1  EnterI5            "ENT5"       
           , f1  EnterI6            "ENT6"       
           , f1  EnterANeg          "ENNA"       
           , f1  EnterXNeg          "ENNX"       
           , f1  EnterI1Neg         "ENN1"       
           , f1  EnterI2Neg         "ENN2"       
           , f1  EnterI3Neg         "ENN3"       
           , f1  EnterI4Neg         "ENN4"       
           , f1  EnterI5Neg         "ENN5"       
           , f1  EnterI6Neg         "ENN6"       
           , f1  IncrementA         "INCA"       
           , f1  IncrementX         "INCX"       
           , f1  IncrementI1        "INC1"       
           , f1  IncrementI2        "INC2"       
           , f1  IncrementI3        "INC3"       
           , f1  IncrementI4        "INC4"       
           , f1  IncrementI5        "INC5"       
           , f1  IncrementI6        "INC6"       
           , f1  DecrementA         "DECA"       
           , f1  DecrementX         "DECX"       
           , f1  DecrementI1        "DEC1"       
           , f1  DecrementI2        "DEC2"       
           , f1  DecrementI3        "DEC3"       
           , f1  DecrementI4        "DEC4"       
           , f1  DecrementI5        "DEC5"       
           , f1  DecrementI6        "DEC6"       
           , f2  CompareA           "CMPA"       
           , f2  CompareX           "CMPX"       
           , f2  CompareI1          "CMP1"       
           , f2  CompareI2          "CMP2"       
           , f2  CompareI3          "CMP3"       
           , f2  CompareI4          "CMP4"       
           , f2  CompareI5          "CMP5"       
           , f2  CompareI6          "CMP6"       
           , f1  Jump               "JMP"        
           , f1  JumpSaveJ          "JSJ"        
           , f1  JumpOnOverflow     "JOV"        
           , f1  JumpOnNoOverflow   "JNOV"       
           , f1  JumpOnLess         "JL"         
           , f1  JumpOnEqual        "JE"         
           , f1  JumpOnGreater      "JG"         
           , f1  JumpOnGreaterEqual "JGE"        
           , f1  JumpOnUnEqual      "JNE"        
           , f1  JumpOnLessEqual    "JLE"        
           , f1  JumpANegative      "JAN"        
           , f1  JumpAZero          "JAZ"        
           , f1  JumpAPositive      "JAP"        
           , f1  JumpANonNegative   "JANN"       
           , f1  JumpANonZero       "JANZ"       
           , f1  JumpANonPositive   "JANP"       
           , f1  JumpXNegative      "JXN"        
           , f1  JumpXZero          "JXZ"        
           , f1  JumpXPositive      "JXP"        
           , f1  JumpXNonNegative   "JXNN"       
           , f1  JumpXNonZero       "JXNZ"       
           , f1  JumpXNonPositive   "JXNP"       
           , f1  JumpI1Negative     "J1N"        
           , f1  JumpI1Zero         "J1Z"        
           , f1  JumpI1Positive     "J1P"        
           , f1  JumpI1NonNegative  "J1NN"       
           , f1  JumpI1NonZero      "J1NZ"       
           , f1  JumpI1NonPositive  "J1NP"       
           , f1  JumpI2Negative     "J2N"        
           , f1  JumpI2Zero         "J2Z"        
           , f1  JumpI2Positive     "J2P"        
           , f1  JumpI2NonNegative  "J2NN"       
           , f1  JumpI2NonZero      "J2NZ"       
           , f1  JumpI2NonPositive  "J2NP"       
           , f1  JumpI3Negative     "J3N"        
           , f1  JumpI3Zero         "J3Z"        
           , f1  JumpI3Positive     "J3P"        
           , f1  JumpI3NonNegative  "J3NN"       
           , f1  JumpI3NonZero      "J3NZ"       
           , f1  JumpI3NonPositive  "J3NP"      
           , f1  JumpI4Negative     "J4N"        
           , f1  JumpI4Zero         "J4Z"        
           , f1  JumpI4Positive     "J4P"        
           , f1  JumpI4NonNegative  "J4NN"       
           , f1  JumpI4NonZero      "J4NZ"       
           , f1  JumpI4NonPositive  "J4NP"       
           , f1  JumpI5Negative     "J5N"       
           , f1  JumpI5Zero         "J5Z"        
           , f1  JumpI5Positive     "J5P"        
           , f1  JumpI5NonNegative  "J5NN"       
           , f1  JumpI5NonZero      "J5NZ"       
           , f1  JumpI5NonPositive  "J5NP"       
           , f1  JumpI6Negative     "J6N"        
           , f1  JumpI6Zero         "J6Z"        
           , f1  JumpI6Positive     "J6P"        
           , f1  JumpI6NonNegative  "J6NN"       
           , f1  JumpI6NonZero      "J6NZ"       
           , f1  JumpI6NonPositive  "J6NP"       
           , f1  ShiftA             "SA"         
           , f1  ShiftX             "SX"         
           , f1  ShiftACircular     "SAC"        
           , f1  ShiftXCircular     "SAX"        
           , f0 SwapAX              "SWAP"       
           , f0 MoveXI1             "MOVX1"      
           , f0 MoveXI2             "MOVX2"      
           , f0 MoveXI3             "MOVX3"      
           , f0 MoveXI4             "MOVX4"      
           , f0 MoveXI5             "MOVX5"      
           , f0 MoveXI6             "MOVX6"      
           , f0 NoOperation         "NOP"        
           ]

tokenize : String -> List Token
tokenize s = case getToken s of
                 Ok (s1,t)   -> t :: (tokenize s1)
                 Err NoMatch -> case String.uncons s of
                                    Nothing -> []
                                    Just (c,s2) -> tokenize s2
                 Err StringEmpty -> []
