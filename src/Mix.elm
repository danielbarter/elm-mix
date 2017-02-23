module Mix exposing ( Memory
                    , read
                    , Mix
                    , instructionTransition
                    , Tag(..)
                    , MetaData
                    , MetaMemory
                    , readMeta
                    )

import Dict
import Instructions exposing (..)
import Atom exposing (..)


type Tag = Number
         | Instruction

type alias MetaData = Tag

type alias Memory = Dict.Dict Address Word
type alias MetaMemory = Dict.Dict Address MetaData

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
instructionTransition i s =
    case i of
        LoadA adr masks
            -> { s | a = copy masks (read adr s.mem) s.a }
        LoadX adr masks
            -> { s | x = copy masks (read adr s.mem) s.x }
        LoadI1 adr masks
            -> { s | i1 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i1 }
        LoadI2 adr masks
            -> { s | i2 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i2 }
        LoadI3 adr masks
            -> { s | i3 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i3 }
        LoadI4 adr masks
            -> { s | i4 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i4 }
        LoadI5 adr masks
            -> { s | i5 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i5 }
        LoadI6 adr masks
            -> { s | i6 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i6 }
        LoadANeg adr masks
            -> { s | a = copy masks (flipSignWord <| read adr s.mem) s.a }
        LoadXNeg adr masks
            -> { s | x = copy masks (flipSignWord <| read adr s.mem) s.x }
        LoadI1Neg adr masks
            -> { s | i1 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i1 }
        LoadI2Neg adr masks
            -> { s | i2 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i2 }
        LoadI3Neg adr masks
            -> { s | i3 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i3 }
        LoadI4Neg adr masks
            -> { s | i4 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i4 }
        LoadI5Neg adr masks
            -> { s | i5 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i5 }
        LoadI6Neg adr masks
            -> { s | i6 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i6 }
        StoreA adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks s.a <| read adr s.mem)
                     s.mem
               }
        StoreX adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks s.x <| read adr s.mem)
                     s.mem
               }
        StoreI1 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i1) <| read adr s.mem)
                     s.mem
               }
        StoreI2 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i2) <| read adr s.mem)
                     s.mem
               }
        StoreI3 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i3) <| read adr s.mem)
                     s.mem
               }
        StoreI4 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i4) <| read adr s.mem)
                     s.mem
               }
        StoreI5 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i5) <| read adr s.mem)
                     s.mem
               }
        StoreI6 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i6) <| read adr s.mem)
                     s.mem
               }
        StoreJ adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.j) <| read adr s.mem)
                     s.mem
               }
        StoreZero adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks zeroWord <| read adr s.mem)
                     s.mem
               }
        Add adr masks
            -> let (t,r) = op (+) masks s.a <| read adr s.mem
               in { s | a = r
                      , overflow = t
                  }
        Sub adr masks
            -> let (t,r) = op (-) masks s.a <| read adr s.mem
               in { s | a = r
                      , overflow = t
                  }
        AddX masks
            -> let (t,r) = op (+) masks s.a <| s.x
               in { s | a = r
                      , overflow = t
                  }
        SubX masks
            -> let (t,r) = op (-) masks s.a <| s.x
               in { s | a = r
                  , overflow = t
                  }
        EnterA adr
            -> let (t,r) = intToWord adr s.a
               in { s | a = r
                  , overflow = t
                  }
        EnterX adr
            -> let (t,r) = intToWord adr s.x
               in { s | x = r
                  , overflow = t
                  }
        EnterI1 adr
            -> let (t,r) = intToSmallWord adr s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        EnterI2 adr
            -> let (t,r) = intToSmallWord adr s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        EnterI3 adr
            -> let (t,r) = intToSmallWord adr s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        EnterI4 adr
            -> let (t,r) = intToSmallWord adr s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        EnterI5 adr
            -> let (t,r) = intToSmallWord adr s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        EnterI6 adr
            -> let (t,r) = intToSmallWord adr s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        EnterANeg adr
            -> let (t,r) = intToWord (negate adr) s.a
               in { s | a = r
                  , overflow = t
                  }
        EnterXNeg adr
            -> let (t,r) = intToWord (negate adr) s.x
               in { s | x = r
                  , overflow = t
                  }
        EnterI1Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        EnterI2Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        EnterI3Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        EnterI4Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        EnterI5Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        EnterI6Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        IncrementA adr
            -> let (t,r) = intToWord ( (wordValue s.a) + adr) s.a
               in { s | a = r
                  , overflow = t
                  }
        IncrementX adr
            -> let (t,r) = intToWord ( (wordValue s.x) + adr) s.x
               in { s | x = r
                  , overflow = t
                  }
        IncrementI1 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i1) + adr) s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        IncrementI2 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i2) + adr) s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        IncrementI3 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i3) + adr) s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        IncrementI4 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i4) + adr) s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        IncrementI5 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i5) + adr) s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        IncrementI6 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i6) + adr) s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        DecrementA adr
            -> let (t,r) = intToWord ( (wordValue s.a) - adr) s.a
               in { s | a = r
                  , overflow = t
                  }
        DecrementX adr
            -> let (t,r) = intToWord ( (wordValue s.x) - adr) s.x
               in { s | x = r
                  , overflow = t
                  }
        DecrementI1 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i1) - adr) s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        DecrementI2 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i2) - adr) s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        DecrementI3 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i3) - adr) s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        DecrementI4 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i4) - adr) s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        DecrementI5 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i5) - adr) s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        DecrementI6 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i6) - adr) s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        CompareA adr masks
            -> let c = comp masks s.a <| read adr s.mem
               in { s | comparison = c }
        CompareX adr masks
            -> let c = comp masks s.x <| read adr s.mem
               in { s | comparison = c }
        CompareI1 adr masks
            -> let c = comp masks (wordExpand s.i1) <| read adr s.mem
               in { s | comparison = c }
        CompareI2 adr masks
            -> let c = comp masks (wordExpand s.i2) <| read adr s.mem
               in { s | comparison = c }
        CompareI3 adr masks
            -> let c = comp masks (wordExpand s.i3) <| read adr s.mem
               in { s | comparison = c }
        CompareI4 adr masks
            -> let c = comp masks (wordExpand s.i4) <| read adr s.mem
               in { s | comparison = c }
        CompareI5 adr masks
            -> let c = comp masks (wordExpand s.i5) <| read adr s.mem
               in { s | comparison = c }
        CompareI6 adr masks
            -> let c = comp masks (wordExpand s.i6) <| read adr s.mem
               in { s | comparison = c }
        Jump adr
            -> let (t,newJ) = intToSmallWord s.p s.j
               in { s
                  | p = adr
                  , j = newJ
                  }
        JumpSaveJ adr
            -> { s | p = adr }
        JumpOnOverflow adr
            -> if s.overflow == Overflow
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       , overflow = Good
                       }
               else s
        JumpOnNoOverflow adr
            -> if s.overflow == Good
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else { s | overflow = Good }
        JumpOnLess adr
            -> if s.comparison == L
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnEqual adr
            -> if s.comparison == E
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnGreater adr
            -> if s.comparison == G
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnGreaterEqual adr
            -> if (s.comparison == G) || (s.comparison == E)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnUnEqual adr
            -> if (s.comparison == L) || (s.comparison == G)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnLessEqual adr
            -> if (s.comparison == L) || (s.comparison == E)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANegative adr
            -> if (wordValue s.a < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpAZero adr
            -> if (wordValue s.a == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpAPositive adr
            -> if (wordValue s.a > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANonNegative adr
            -> if (wordValue s.a > 0) || (wordValue s.a == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANonZero adr
            -> if (wordValue s.a < 0) || (wordValue s.a > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANonPositive adr
            -> if (wordValue s.a < 0) || (wordValue s.a == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNegative adr
            -> if (wordValue s.x < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXZero adr
            -> if (wordValue s.x == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXPositive adr
            -> if (wordValue s.x > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNonNegative adr
            -> if (wordValue s.x > 0) || (wordValue s.x == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNonZero adr
            -> if (wordValue s.x < 0) || (wordValue s.x > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNonPositive adr
            -> if (wordValue s.x < 0) || (wordValue s.x == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1Negative adr
            -> if (smallWordValue s.i1 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1Zero adr
            -> if (smallWordValue s.i1 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1Positive adr
            -> if (smallWordValue s.i1 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1NonNegative adr
            -> if (smallWordValue s.i1 > 0) || (smallWordValue s.i1 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1NonZero adr
            -> if (smallWordValue s.i1 < 0) || (smallWordValue s.i1 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1NonPositive adr
            -> if (smallWordValue s.i1 < 0) || (smallWordValue s.i1 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2Negative adr
            -> if (smallWordValue s.i2 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2Zero adr
            -> if (smallWordValue s.i2 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2Positive adr
            -> if (smallWordValue s.i2 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2NonNegative adr
            -> if (smallWordValue s.i2 > 0) || (smallWordValue s.i2 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2NonZero adr
            -> if (smallWordValue s.i2 < 0) || (smallWordValue s.i2 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2NonPositive adr
            -> if (smallWordValue s.i2 < 0) || (smallWordValue s.i2 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3Negative adr
            -> if (smallWordValue s.i3 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3Zero adr
            -> if (smallWordValue s.i3 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3Positive adr
            -> if (smallWordValue s.i3 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3NonNegative adr
            -> if (smallWordValue s.i3 > 0) || (smallWordValue s.i3 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3NonZero adr
            -> if (smallWordValue s.i3 < 0) || (smallWordValue s.i3 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3NonPositive adr
            -> if (smallWordValue s.i3 < 0) || (smallWordValue s.i3 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4Negative adr
            -> if (smallWordValue s.i4 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4Zero adr
            -> if (smallWordValue s.i4 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4Positive adr
            -> if (smallWordValue s.i4 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4NonNegative adr
            -> if (smallWordValue s.i4 > 0) || (smallWordValue s.i4 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4NonZero adr
            -> if (smallWordValue s.i4 < 0) || (smallWordValue s.i4 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4NonPositive adr
            -> if (smallWordValue s.i4 < 0) || (smallWordValue s.i4 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5Negative adr
            -> if (smallWordValue s.i5 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5Zero adr
            -> if (smallWordValue s.i5 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5Positive adr
            -> if (smallWordValue s.i5 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5NonNegative adr
            -> if (smallWordValue s.i5 > 0) || (smallWordValue s.i5 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5NonZero adr
            -> if (smallWordValue s.i5 < 0) || (smallWordValue s.i5 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5NonPositive adr
            -> if (smallWordValue s.i5 < 0) || (smallWordValue s.i5 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6Negative adr
            -> if (smallWordValue s.i6 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6Zero adr
            -> if (smallWordValue s.i6 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6Positive adr
            -> if (smallWordValue s.i6 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6NonNegative adr
            -> if (smallWordValue s.i6 > 0) || (smallWordValue s.i6 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6NonZero adr
            -> if (smallWordValue s.i6 < 0) || (smallWordValue s.i6 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6NonPositive adr
            -> if (smallWordValue s.i6 < 0) || (smallWordValue s.i6 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        ShiftA adr
            -> { s | a = shift adr s.a }
        ShiftX adr
            -> { s | x = shift adr s.x }
        ShiftACircular adr
            -> { s | a = shiftCircular adr s.a }
        ShiftXCircular adr
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



