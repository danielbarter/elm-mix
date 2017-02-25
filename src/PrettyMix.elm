module PrettyMix exposing (..)

import Mix exposing (..)
import Atom exposing (..)
import Instruction exposing (..)

ppA w     = "A  = " ++ ( toString <| wordValue w)
ppX w     = "X  = " ++ ( toString <| wordValue w)
ppI1 w    = "I1 = " ++ ( toString <| wordValue w)
ppI2 w    = "I2 = " ++ ( toString <| wordValue w)
ppI3 w    = "I3 = " ++ ( toString <| wordValue w)
ppI4 w    = "I4 = " ++ ( toString <| wordValue w)
ppI5 w    = "I5 = " ++ ( toString <| wordValue w)
ppI6 w    = "I6 = " ++ ( toString <| wordValue w)
ppJ  w    = "J  = " ++ ( toString <| wordValue w)
ppCount p = "p  = " ++ ( toString p )

{-
ppMemLoc : Address -> Memory -> MetaMemory -> String
ppMemLoc a mem meta =
    let w = read a mem
        t = readMeta a meta
    in case t of
           Number -> (toString a) ++ ": " ++ (toString <| wordValue w)
           Instruction -> (toString a) ++ ": "
-}
