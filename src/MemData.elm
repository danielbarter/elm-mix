module MemData exposing ( CurrentInstruction
                        , MemData
                        , totalMemData
                        , ppMemData
                        , ppWord
                        , ppJump
                        , ppSmallWord
                        , ppOverflow
                        , ppComparision
                        )

import Atom exposing (..)
import Instruction exposing (..)
import Mix exposing (..)

import Dict
import Color exposing (..)

type alias CurrentInstruction = Bool

type alias MemData = ( Address
                     , Maybe String
                     , MemoryTag
                     , Int
                     , Result DecodeError StaticInstructionClean
                     , CurrentInstruction
                     )

memData : Mix -> Address -> MemData
memData m a =
    ( a
    , Dict.get a m.reverseSymbolTable
    , readMeta a m.meta
    , wordValue <| read a m.mem
    , Result.map cleanStatic <| decodeInstruction <| unpack <| read a m.mem
    , m.p == a
    )

totalMemData : Mix -> List MemData
totalMemData m = List.map (memData m) <| Dict.keys m.mem


ppWord : Word -> (String,Color,Color)
ppWord w = (toString <| wordValue w,lightCharcoal,black)

ppSmallWord : SmallWord -> (String,Color,Color)
ppSmallWord w = (toString <| smallWordValue w,darkCharcoal,white)

ppJump : SmallWord -> (String,Color,Color)
ppJump w = (toString <| smallWordValue w,darkBlue,white)

ppOverflow : OverflowToggle -> (String,Color,Color)
ppOverflow t =
    case t of
        Overflow -> ("Overflow",darkRed,white)
        Good     -> ("Good",darkGreen,white)
        Ignored ->  ("Fuck!",black,white)

ppComparision : ComparisonIndicator -> (String,Color,Color)
ppComparision t =
    case t of
        L -> ("<",darkRed,white)
        E -> ("=",darkGrey,white)
        G -> (">",darkGreen,white)



ppMaybeAddress : Mix -> Maybe Address -> String
ppMaybeAddress mix a =
    case a of
        Nothing -> ""
        Just x -> case Dict.get x mix.reverseSymbolTable of
                      Nothing -> (toString x)
                      Just l  -> (toString x) ++ "|" ++ l

ppMaybeIndex : Maybe Index -> String
ppMaybeIndex i =
    case i of
        Nothing -> ""
        Just x  -> case x of
                       0 -> ""
                       _ -> "+" ++ toString x

ppMaybeMasks : Maybe Masks -> String
ppMaybeMasks m =
    case m of
        Nothing -> ""
        Just x -> case ((value << masksToByte ) x) of
                      0 -> ""
                      n -> "/" ++ (toString n)


ppStaticInstructionClean : Mix -> StaticInstructionClean -> String
ppStaticInstructionClean mix (a,i,m,t) =
    let st = ppTag t
        sa = ppMaybeAddress mix a
        si = ppMaybeIndex i
        sm = ppMaybeMasks m
    in  String.join " " [sm,st,sa,si]



ppPrefix : Address -> Maybe String -> String
ppPrefix a l =
    let pref = (toString a)
    in case l of
           Nothing -> pref ++ " "
           Just x  -> pref ++ ":" ++ x ++ " "

ppValue : Mix -> Int -> String
ppValue mix v =
    case Dict.get v mix.reverseSymbolTable of
        Nothing -> (toString v)
        Just l ->  (toString v) ++ "|" ++ l

ppMemData : Mix -> MemData -> (String,Color,Color)
ppMemData mix d =
    let (a,l,t,v,i,b) = d
        prefix = ppPrefix a l
        vv = ppValue mix v
    in case t of
           Number -> if b
                     then (prefix ++ vv,darkOrange,white)
                     else (prefix ++ vv,lightCharcoal,black)
           Instruction
               -> case i of
                      Err err -> ppMemData mix (a,l,Number,v,i,b)
                      Ok inst -> let s = ppStaticInstructionClean mix inst
                                 in if b
                                    then (prefix ++ s,darkOrange,white)
                                    else (prefix ++ s,lightCharcoal,black)



