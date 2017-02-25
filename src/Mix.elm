module Mix exposing (..)

import Dict
import Instruction exposing (..)
import Atom exposing (..)

type MemoryTag = Number
               | Instruction

type alias MetaData = MemoryTag

type alias Memory = Dict.Dict Address Word
type alias MetaMemory = Dict.Dict Address MetaData

-- the default meta data is a number tag
    
readMeta : Address -> MetaMemory -> MetaData
readMeta a meta = Maybe.withDefault Number <| Dict.get a meta

-- the default memory value is zeroWord

read : Address -> Memory -> Word
read a mem = Maybe.withDefault zeroWord <| Dict.get a mem



type alias Mix = { a   : Word
                 , x   : Word
                 , i1  : SmallWord
                 , i2  : SmallWord
                 , i3  : SmallWord
                 , i4  : SmallWord
                 , i5  : SmallWord
                 , i6  : SmallWord
                 , j   : SmallWord
                 , p   : Address -- program counter
                 , mem : Memory
                 , meta : MetaMemory
                 , overflow : OverflowToggle
                 , comparison : ComparisonIndicator
                 }

{-

design point:

we are not implementing multiplication and division at a machine level.
Instead, we have AddX and SubX.
Multiplication / Division are easily implemented as routines.

-}


instructionTransition : DynamicInstruction -> Mix -> Mix
instructionTransition (adr,masks,i) s =
    case i of
        LoadA
            -> { s | a = copy masks (read adr s.mem) s.a }
        LoadX 
            -> { s | x = copy masks (read adr s.mem) s.x }
        LoadI1 
            -> { s | i1 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i1 }
        LoadI2
            -> { s | i2 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i2 }
        LoadI3 
            -> { s | i3 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i3 }
        LoadI4 
            -> { s | i4 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i4 }
        LoadI5 
            -> { s | i5 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i5 }
        LoadI6 
            -> { s | i6 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i6 }
        LoadANeg 
            -> { s | a = copy masks (flipSignWord <| read adr s.mem) s.a }
        LoadXNeg 
            -> { s | x = copy masks (flipSignWord <| read adr s.mem) s.x }
        LoadI1Neg 
            -> { s | i1 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i1 }
        LoadI2Neg 
            -> { s | i2 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i2 }
        LoadI3Neg 
            -> { s | i3 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i3 }
        LoadI4Neg 
            -> { s | i4 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i4 }
        LoadI5Neg 
            -> { s | i5 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i5 }
        LoadI6Neg 
            -> { s | i6 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i6 }
        StoreA 
            -> { s | mem = Dict.insert adr
                     (copy masks s.a <| read adr s.mem)
                     s.mem
               }
        StoreX 
            -> { s | mem = Dict.insert adr
                     (copy masks s.x <| read adr s.mem)
                     s.mem
               }
        StoreI1 
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i1) <| read adr s.mem)
                     s.mem
               }
        StoreI2 
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i2) <| read adr s.mem)
                     s.mem
               }
        StoreI3 
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i3) <| read adr s.mem)
                     s.mem
               }
        StoreI4 
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i4) <| read adr s.mem)
                     s.mem
               }
        StoreI5 
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i5) <| read adr s.mem)
                     s.mem
               }
        StoreI6 
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i6) <| read adr s.mem)
                     s.mem
               }
        StoreJ 
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.j) <| read adr s.mem)
                     s.mem
               }
        StoreZero 
            -> { s | mem = Dict.insert adr
                     (copy masks zeroWord <| read adr s.mem)
                     s.mem
               }
        Add 
            -> let (t,r) = op (+) masks s.a <| read adr s.mem
               in { s | a = r
                      , overflow = t
                  }
        Sub 
            -> let (t,r) = op (-) masks s.a <| read adr s.mem
               in { s | a = r
                      , overflow = t
                  }
        AddX 
            -> let (t,r) = op (+) masks s.a <| s.x
               in { s | a = r
                      , overflow = t
                  }
        SubX 
            -> let (t,r) = op (-) masks s.a <| s.x
               in { s | a = r
                  , overflow = t
                  }
        EnterA 
            -> let (t,r) = intToWord adr s.a
               in { s | a = r
                  , overflow = t
                  }
        EnterX 
            -> let (t,r) = intToWord adr s.x
               in { s | x = r
                  , overflow = t
                  }
        EnterI1 
            -> let (t,r) = intToSmallWord adr s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        EnterI2 
            -> let (t,r) = intToSmallWord adr s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        EnterI3 
            -> let (t,r) = intToSmallWord adr s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        EnterI4 
            -> let (t,r) = intToSmallWord adr s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        EnterI5 
            -> let (t,r) = intToSmallWord adr s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        EnterI6 
            -> let (t,r) = intToSmallWord adr s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        EnterANeg 
            -> let (t,r) = intToWord (negate adr) s.a
               in { s | a = r
                  , overflow = t
                  }
        EnterXNeg 
            -> let (t,r) = intToWord (negate adr) s.x
               in { s | x = r
                  , overflow = t
                  }
        EnterI1Neg 
            -> let (t,r) = intToSmallWord (negate adr) s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        EnterI2Neg 
            -> let (t,r) = intToSmallWord (negate adr) s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        EnterI3Neg
            -> let (t,r) = intToSmallWord (negate adr) s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        EnterI4Neg 
            -> let (t,r) = intToSmallWord (negate adr) s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        EnterI5Neg 
            -> let (t,r) = intToSmallWord (negate adr) s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        EnterI6Neg 
            -> let (t,r) = intToSmallWord (negate adr) s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        IncrementA 
            -> let (t,r) = intToWord ( (wordValue s.a) + adr) s.a
               in { s | a = r
                  , overflow = t
                  }
        IncrementX 
            -> let (t,r) = intToWord ( (wordValue s.x) + adr) s.x
               in { s | x = r
                  , overflow = t
                  }
        IncrementI1 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i1) + adr) s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        IncrementI2 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i2) + adr) s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        IncrementI3 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i3) + adr) s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        IncrementI4 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i4) + adr) s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        IncrementI5 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i5) + adr) s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        IncrementI6 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i6) + adr) s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        DecrementA 
            -> let (t,r) = intToWord ( (wordValue s.a) - adr) s.a
               in { s | a = r
                  , overflow = t
                  }
        DecrementX 
            -> let (t,r) = intToWord ( (wordValue s.x) - adr) s.x
               in { s | x = r
                  , overflow = t
                  }
        DecrementI1
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i1) - adr) s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        DecrementI2 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i2) - adr) s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        DecrementI3 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i3) - adr) s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        DecrementI4 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i4) - adr) s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        DecrementI5 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i5) - adr) s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        DecrementI6 
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i6) - adr) s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        CompareA 
            -> let c = comp masks s.a <| read adr s.mem
               in { s | comparison = c }
        CompareX 
            -> let c = comp masks s.x <| read adr s.mem
               in { s | comparison = c }
        CompareI1 
            -> let c = comp masks (wordExpand s.i1) <| read adr s.mem
               in { s | comparison = c }
        CompareI2 
            -> let c = comp masks (wordExpand s.i2) <| read adr s.mem
               in { s | comparison = c }
        CompareI3 
            -> let c = comp masks (wordExpand s.i3) <| read adr s.mem
               in { s | comparison = c }
        CompareI4 
            -> let c = comp masks (wordExpand s.i4) <| read adr s.mem
               in { s | comparison = c }
        CompareI5 
            -> let c = comp masks (wordExpand s.i5) <| read adr s.mem
               in { s | comparison = c }
        CompareI6 
            -> let c = comp masks (wordExpand s.i6) <| read adr s.mem
               in { s | comparison = c }
        Jump 
            -> let (t,newJ) = intToSmallWord s.p s.j
               in { s
                  | p = adr
                  , j = newJ
                  }
        JumpSaveJ 
            -> { s | p = adr }
        JumpOnOverflow 
            -> if s.overflow == Overflow
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       , overflow = Good
                       }
               else s
        JumpOnNoOverflow 
            -> if s.overflow == Good
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else { s | overflow = Good }
        JumpOnLess 
            -> if s.comparison == L
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnEqual 
            -> if s.comparison == E
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnGreater 
            -> if s.comparison == G
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnGreaterEqual 
            -> if (s.comparison == G) || (s.comparison == E)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnUnEqual 
            -> if (s.comparison == L) || (s.comparison == G)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnLessEqual 
            -> if (s.comparison == L) || (s.comparison == E)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANegative 
            -> if (wordValue s.a < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpAZero 
            -> if (wordValue s.a == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpAPositive 
            -> if (wordValue s.a > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANonNegative 
            -> if (wordValue s.a > 0) || (wordValue s.a == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANonZero 
            -> if (wordValue s.a < 0) || (wordValue s.a > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANonPositive 
            -> if (wordValue s.a < 0) || (wordValue s.a == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNegative 
            -> if (wordValue s.x < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXZero 
            -> if (wordValue s.x == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXPositive 
            -> if (wordValue s.x > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNonNegative 
            -> if (wordValue s.x > 0) || (wordValue s.x == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNonZero 
            -> if (wordValue s.x < 0) || (wordValue s.x > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNonPositive 
            -> if (wordValue s.x < 0) || (wordValue s.x == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1Negative 
            -> if (smallWordValue s.i1 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1Zero 
            -> if (smallWordValue s.i1 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1Positive 
            -> if (smallWordValue s.i1 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1NonNegative 
            -> if (smallWordValue s.i1 > 0) || (smallWordValue s.i1 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1NonZero 
            -> if (smallWordValue s.i1 < 0) || (smallWordValue s.i1 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1NonPositive 
            -> if (smallWordValue s.i1 < 0) || (smallWordValue s.i1 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2Negative 
            -> if (smallWordValue s.i2 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2Zero 
            -> if (smallWordValue s.i2 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2Positive 
            -> if (smallWordValue s.i2 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2NonNegative 
            -> if (smallWordValue s.i2 > 0) || (smallWordValue s.i2 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2NonZero 
            -> if (smallWordValue s.i2 < 0) || (smallWordValue s.i2 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2NonPositive 
            -> if (smallWordValue s.i2 < 0) || (smallWordValue s.i2 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3Negative 
            -> if (smallWordValue s.i3 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3Zero 
            -> if (smallWordValue s.i3 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3Positive 
            -> if (smallWordValue s.i3 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3NonNegative 
            -> if (smallWordValue s.i3 > 0) || (smallWordValue s.i3 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3NonZero 
            -> if (smallWordValue s.i3 < 0) || (smallWordValue s.i3 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3NonPositive 
            -> if (smallWordValue s.i3 < 0) || (smallWordValue s.i3 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4Negative 
            -> if (smallWordValue s.i4 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4Zero 
            -> if (smallWordValue s.i4 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4Positive 
            -> if (smallWordValue s.i4 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4NonNegative 
            -> if (smallWordValue s.i4 > 0) || (smallWordValue s.i4 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4NonZero 
            -> if (smallWordValue s.i4 < 0) || (smallWordValue s.i4 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4NonPositive 
            -> if (smallWordValue s.i4 < 0) || (smallWordValue s.i4 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5Negative 
            -> if (smallWordValue s.i5 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5Zero 
            -> if (smallWordValue s.i5 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5Positive 
            -> if (smallWordValue s.i5 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5NonNegative 
            -> if (smallWordValue s.i5 > 0) || (smallWordValue s.i5 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5NonZero 
            -> if (smallWordValue s.i5 < 0) || (smallWordValue s.i5 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5NonPositive 
            -> if (smallWordValue s.i5 < 0) || (smallWordValue s.i5 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6Negative 
            -> if (smallWordValue s.i6 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6Zero 
            -> if (smallWordValue s.i6 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6Positive 
            -> if (smallWordValue s.i6 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6NonNegative 
            -> if (smallWordValue s.i6 > 0) || (smallWordValue s.i6 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6NonZero 
            -> if (smallWordValue s.i6 < 0) || (smallWordValue s.i6 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6NonPositive 
            -> if (smallWordValue s.i6 < 0) || (smallWordValue s.i6 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        ShiftA 
            -> { s | a = shift adr s.a }
        ShiftX 
            -> { s | x = shift adr s.x }
        ShiftACircular 
            -> { s | a = shiftCircular adr s.a }
        ShiftXCircular 
            -> { s | x = shiftCircular adr s.x }
        SwapAX
            -> { s | a = s.x , x = s.a }
        MoveXI1
            -> { s | i1 = wordContract s.x }
        MoveXI2
            -> { s | i2 = wordContract s.x }
        MoveXI3
            -> { s | i3 = wordContract s.x }
        MoveXI4
            -> { s | i4 = wordContract s.x }
        MoveXI5
            -> { s | i5 = wordContract s.x }
        MoveXI6
            -> { s | i6 = wordContract s.x }
        NoOperation
            -> s
        Halt
            -> s
