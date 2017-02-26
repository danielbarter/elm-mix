module PrettyMix exposing (ppMix)

import Mix exposing (..)
import Atom exposing (..)
import Instruction exposing (..)
import Dict

ppA w        = "A  = " ++ ( toString <| wordValue w)
ppX w        = "X  = " ++ ( toString <| wordValue w)
ppI1 w       = "I1 = " ++ ( toString <| smallWordValue w)
ppI2 w       = "I2 = " ++ ( toString <| smallWordValue w)
ppI3 w       = "I3 = " ++ ( toString <| smallWordValue w)
ppI4 w       = "I4 = " ++ ( toString <| smallWordValue w)
ppI5 w       = "I5 = " ++ ( toString <| smallWordValue w)
ppI6 w       = "I6 = " ++ ( toString <| smallWordValue w)
ppJ  w       = "J  = " ++ ( toString <| smallWordValue w)
ppCount p    = "p  = " ++ ( toString p )
ppOverflow t = "ov = " ++ ( toString t )
ppComp c     = "cp = " ++ ( toString c )

ppMemLoc : Memory -> MetaMemory -> Address -> String
ppMemLoc mem meta a =
    let w = read a mem
        t = readMeta a meta
    in case t of
           Number -> (toString a) ++ ": " ++ (toString <| wordValue w)
           Instruction -> case decodeInstruction <| unpack w of
                              Ok i  -> (toString a) ++ ": " ++ (ppInstruction i)
                              Err _ -> (toString a)
                                       ++ ": "
                                       ++ (toString <| wordValue w)

ppMem : Memory -> MetaMemory -> List String
ppMem mem meta = List.map (ppMemLoc mem meta) <| Dict.keys mem

ppMix : Mix -> List String
ppMix m = [ ppA m.a
          , ppX m.x
          , ppI1 m.i1
          , ppI2 m.i2
          , ppI3 m.i3
          , ppI4 m.i4
          , ppI5 m.i5
          , ppI6 m.i6
          , ppJ m.j
          , ppCount m.p
          , ppOverflow m.overflow
          , ppComp m.comparison
          ] ++ (ppMem m.mem m.meta)



ppMaskInstruction : StaticInstruction -> String
ppMaskInstruction (a,i,ms,t) = (toString <| value <| masksToByte  ms) ++ " " ++
                               (toString t) ++ " " ++
                               (toString a) ++ " " ++ (toString i)

ppNoMaskInstruction : StaticInstruction -> String
ppNoMaskInstruction (a,i,ms,t) = (toString t) ++ " " ++
                                 (toString a) ++ " " ++ (toString i)


ppInstruction : StaticInstruction -> String
ppInstruction inst =
    let (a,i,ms,t) = inst
    in case t of
          LoadA                -> ppMaskInstruction inst
          LoadX                -> ppMaskInstruction inst
          LoadI1               -> ppMaskInstruction inst
          LoadI2               -> ppMaskInstruction inst
          LoadI3               -> ppMaskInstruction inst
          LoadI4               -> ppMaskInstruction inst
          LoadI5               -> ppMaskInstruction inst
          LoadI6               -> ppMaskInstruction inst
          LoadANeg             -> ppMaskInstruction inst
          LoadXNeg             -> ppMaskInstruction inst
          LoadI1Neg            -> ppMaskInstruction inst
          LoadI2Neg            -> ppMaskInstruction inst
          LoadI3Neg            -> ppMaskInstruction inst
          LoadI4Neg            -> ppMaskInstruction inst
          LoadI5Neg            -> ppMaskInstruction inst
          LoadI6Neg            -> ppMaskInstruction inst
          StoreA               -> ppMaskInstruction inst
          StoreX               -> ppMaskInstruction inst
          StoreI1              -> ppMaskInstruction inst
          StoreI2              -> ppMaskInstruction inst
          StoreI3              -> ppMaskInstruction inst
          StoreI4              -> ppMaskInstruction inst
          StoreI5              -> ppMaskInstruction inst
          StoreI6              -> ppMaskInstruction inst
          StoreJ               -> ppMaskInstruction inst
          StoreZero            -> ppMaskInstruction inst
          Add                  -> ppMaskInstruction inst
          Sub                  -> ppMaskInstruction inst
          AddX                 -> ppMaskInstruction inst
          SubX                 -> ppMaskInstruction inst
          EnterA               -> ppNoMaskInstruction inst
          EnterX               -> ppNoMaskInstruction inst
          EnterI1              -> ppNoMaskInstruction inst
          EnterI2              -> ppNoMaskInstruction inst
          EnterI3              -> ppNoMaskInstruction inst
          EnterI4              -> ppNoMaskInstruction inst
          EnterI5              -> ppNoMaskInstruction inst
          EnterI6              -> ppNoMaskInstruction inst
          EnterANeg            -> ppNoMaskInstruction inst
          EnterXNeg            -> ppNoMaskInstruction inst
          EnterI1Neg           -> ppNoMaskInstruction inst
          EnterI2Neg           -> ppNoMaskInstruction inst
          EnterI3Neg           -> ppNoMaskInstruction inst
          EnterI4Neg           -> ppNoMaskInstruction inst
          EnterI5Neg           -> ppNoMaskInstruction inst
          EnterI6Neg           -> ppNoMaskInstruction inst
          IncrementA           -> ppNoMaskInstruction inst
          IncrementX           -> ppNoMaskInstruction inst
          IncrementI1          -> ppNoMaskInstruction inst
          IncrementI2          -> ppNoMaskInstruction inst
          IncrementI3          -> ppNoMaskInstruction inst
          IncrementI4          -> ppNoMaskInstruction inst
          IncrementI5          -> ppNoMaskInstruction inst
          IncrementI6          -> ppNoMaskInstruction inst
          DecrementA           -> ppNoMaskInstruction inst
          DecrementX           -> ppNoMaskInstruction inst
          DecrementI1          -> ppNoMaskInstruction inst
          DecrementI2          -> ppNoMaskInstruction inst
          DecrementI3          -> ppNoMaskInstruction inst
          DecrementI4          -> ppNoMaskInstruction inst
          DecrementI5          -> ppNoMaskInstruction inst
          DecrementI6          -> ppNoMaskInstruction inst
          CompareA             -> ppMaskInstruction inst
          CompareX             -> ppMaskInstruction inst
          CompareI1            -> ppMaskInstruction inst
          CompareI2            -> ppMaskInstruction inst
          CompareI3            -> ppMaskInstruction inst
          CompareI4            -> ppMaskInstruction inst
          CompareI5            -> ppMaskInstruction inst
          CompareI6            -> ppMaskInstruction inst
          Jump                 -> ppNoMaskInstruction inst
          JumpSaveJ            -> ppNoMaskInstruction inst
          JumpOnOverflow       -> ppNoMaskInstruction inst
          JumpOnNoOverflow     -> ppNoMaskInstruction inst
          JumpOnLess           -> ppNoMaskInstruction inst
          JumpOnEqual          -> ppNoMaskInstruction inst
          JumpOnGreater        -> ppNoMaskInstruction inst
          JumpOnGreaterEqual   -> ppNoMaskInstruction inst
          JumpOnUnEqual        -> ppNoMaskInstruction inst
          JumpOnLessEqual      -> ppNoMaskInstruction inst
          JumpANegative        -> ppNoMaskInstruction inst
          JumpAZero            -> ppNoMaskInstruction inst
          JumpAPositive        -> ppNoMaskInstruction inst
          JumpANonNegative     -> ppNoMaskInstruction inst
          JumpANonZero         -> ppNoMaskInstruction inst
          JumpANonPositive     -> ppNoMaskInstruction inst
          JumpXNegative        -> ppNoMaskInstruction inst
          JumpXZero            -> ppNoMaskInstruction inst
          JumpXPositive        -> ppNoMaskInstruction inst
          JumpXNonNegative     -> ppNoMaskInstruction inst
          JumpXNonZero         -> ppNoMaskInstruction inst
          JumpXNonPositive     -> ppNoMaskInstruction inst
          JumpI1Negative       -> ppNoMaskInstruction inst
          JumpI1Zero           -> ppNoMaskInstruction inst
          JumpI1Positive       -> ppNoMaskInstruction inst
          JumpI1NonNegative    -> ppNoMaskInstruction inst
          JumpI1NonZero        -> ppNoMaskInstruction inst
          JumpI1NonPositive    -> ppNoMaskInstruction inst
          JumpI2Negative       -> ppNoMaskInstruction inst
          JumpI2Zero           -> ppNoMaskInstruction inst
          JumpI2Positive       -> ppNoMaskInstruction inst
          JumpI2NonNegative    -> ppNoMaskInstruction inst
          JumpI2NonZero        -> ppNoMaskInstruction inst
          JumpI2NonPositive    -> ppNoMaskInstruction inst
          JumpI3Negative       -> ppNoMaskInstruction inst
          JumpI3Zero           -> ppNoMaskInstruction inst
          JumpI3Positive       -> ppNoMaskInstruction inst
          JumpI3NonNegative    -> ppNoMaskInstruction inst
          JumpI3NonZero        -> ppNoMaskInstruction inst
          JumpI3NonPositive    -> ppNoMaskInstruction inst
          JumpI4Negative       -> ppNoMaskInstruction inst
          JumpI4Zero           -> ppNoMaskInstruction inst
          JumpI4Positive       -> ppNoMaskInstruction inst
          JumpI4NonNegative    -> ppNoMaskInstruction inst
          JumpI4NonZero        -> ppNoMaskInstruction inst
          JumpI4NonPositive    -> ppNoMaskInstruction inst
          JumpI5Negative       -> ppNoMaskInstruction inst
          JumpI5Zero           -> ppNoMaskInstruction inst
          JumpI5Positive       -> ppNoMaskInstruction inst
          JumpI5NonNegative    -> ppNoMaskInstruction inst
          JumpI5NonZero        -> ppNoMaskInstruction inst
          JumpI5NonPositive    -> ppNoMaskInstruction inst
          JumpI6Negative       -> ppNoMaskInstruction inst
          JumpI6Zero           -> ppNoMaskInstruction inst
          JumpI6Positive       -> ppNoMaskInstruction inst
          JumpI6NonNegative    -> ppNoMaskInstruction inst
          JumpI6NonZero        -> ppNoMaskInstruction inst
          JumpI6NonPositive    -> ppNoMaskInstruction inst
          ShiftA               -> ppNoMaskInstruction inst
          ShiftX               -> ppNoMaskInstruction inst
          ShiftACircular       -> ppNoMaskInstruction inst
          ShiftXCircular       -> ppNoMaskInstruction inst
          SwapAX               -> ppNoMaskInstruction inst
          MoveXI1              -> ppNoMaskInstruction inst
          MoveXI2              -> ppNoMaskInstruction inst
          MoveXI3              -> ppNoMaskInstruction inst
          MoveXI4              -> ppNoMaskInstruction inst
          MoveXI5              -> ppNoMaskInstruction inst
          MoveXI6              -> ppNoMaskInstruction inst
          NoOperation          -> ppMaskInstruction inst
          Halt                 -> ppNoMaskInstruction inst
