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



ppInstruction : StaticInstruction -> String
ppInstruction (a,i,ms,t) = (toString <| value <| masksToByte  ms) ++ " " ++
                           (toString t) ++ " " ++
                           (toString a) ++ " " ++ (toString i)
