module TestMixStates exposing (..)

import Mix exposing (..)
import Atom exposing (..)
import Dict


-- test states
testLoad : Mix
testLoad =
    let
        b = masksToByte (On,Off,Off,Off,Off,Off)
        m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 1,b,byte 23))
                          , (1899,(Pos,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = zeroWord
       , x = zeroWord
       , i1 = (Neg,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }


testStore : Mix
testStore =
    let
        b = masksToByte (On,Off,On,Off,Off,Off)
        m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 1,b,byte 25))
                          , (1899,(Pos,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = (Pos,byte 6,byte 7,byte 8,byte 9,byte 0)
       , x = zeroWord
       , i1 = (Neg,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }


testAdd : Mix
testAdd =
    let
        b = masksToByte (On,Off,Off,Off,Off,Off)
        m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 1,b,byte 1))
                          , (1899,(Neg,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = (Pos,byte 1,byte 0,byte 0,byte 0,byte 0)
       , x = zeroWord
       , i1 = (Neg,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }


testEnter : Mix
testEnter =
    let
        m = Dict.fromList [ (0,(Neg,byte 20,byte 0,byte 1,byte 2,byte 54))
                          , (1899,(Neg,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = (Pos,byte 1,byte 0,byte 0,byte 0,byte 0)
       , x = zeroWord
       , i1 = (Neg,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }


testJump : Mix
testJump =
    let
        m = Dict.fromList [ (0,(Pos,byte 0,byte 53,byte 1,byte 0,byte 39))
                          , (1899,(Neg,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = (Pos,byte 1,byte 0,byte 0,byte 0,byte 0)
       , x = zeroWord
       , i1 = (Pos,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }

