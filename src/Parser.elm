{-

This codes parses a sequence of tokens into a list of REL expressions from CodeFunctor

(:label) (/mask) instruction (relative address) (+index)
-}

module Parser exposing ( parse
                       , ParserError(..)
                       )


import Atom exposing (..)
import Tokenizer exposing (..)
import CodeFunctor exposing (..)
import Instruction exposing (..)

type ParserError = NothingRightNow


getInstruction : List Token -> Maybe Tag
getInstruction l =
    case l of
        [] -> Nothing
        t::ts -> case t of
                     InstructionTag i -> Just i
                     _ -> getInstruction ts

getRelativeAddress : List Token -> Maybe RelativeAddress
getRelativeAddress l =
    case l of
        [] -> Nothing
        t::ts -> case t of
                     RelativeTag a -> Just a
                     _ -> getRelativeAddress ts

getRelativeAddressDefault : List Token -> RelativeAddress
getRelativeAddressDefault l =
    Maybe.withDefault (Value 0) <| getRelativeAddress l

getLabel : List Token -> Maybe String
getLabel l =
    case l of
        [] -> Nothing
        t::ts -> case t of
                     LabelTag n -> Just n
                     _ -> getLabel ts

getMask : List Token -> Maybe Int
getMask l =
    case l of
        [] -> Nothing
        t::ts -> case t of
                     MaskTag m -> Just m
                     _ -> getMask ts

getMaskDefault : List Token -> Masks
getMaskDefault l = byteToMasks <| byte <| Maybe.withDefault 0 <| getMask l

getIndex : List Token -> Maybe Int
getIndex l =
    case l of
        [] -> Nothing
        t::ts -> case t of
                     IndexTag i -> Just i
                     _ -> getIndex ts


getIndexDefault : List Token -> Int
getIndexDefault l =
   Maybe.withDefault 0 <| getIndex l


parseLine : List Token -> Result ParserError REL
parseLine l =
    let label = getLabel l
        masks = getMaskDefault l
        tag = getInstruction l
        address = getRelativeAddressDefault l
        index = getIndexDefault l
    in case tag of
           Nothing -> Ok <| Code label <| DataNumber address
           Just i -> Ok <| Code label <| DataInstruction (address,index,masks,i)

parse : List (List Token) -> Result ParserError (List REL)
parse s = distrubuteError <| List.map parseLine s

