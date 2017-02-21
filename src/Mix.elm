module Mix exposing (..)

import Dict

import Atom exposing ( Base
                     , Sign(..)
                     , swap
                     , baseExpand
                     , baseExpandPad
                     , baseContract
                     , mixBase
                     , Byte
                     , byte
                     , zero
                     , zeroWord
                     , zeroSmallWord
                     , value
                     , SmallWord
                     , Word
                     , wordExpand
                     , wordContract
                     , Mask(..)
                     , Masks
                     , maskFilter
                     , copy
                     , wordValue
                     , smallWordValue
                     , byteToMasks
                     , masksToByte
                     , flipSignWord
                     , flipSignSmallWord
                     , OverflowToggle(..)
                     , ComparisonIndicator(..)
                     , op
                     , intToWord
                     , intToSmallWord
                     )
{-

execution cycle:
  unpack instruction
  increment program counter
  decode instruction
  execute instruction

-}


type alias Address = Int
type alias Index = Int
type alias Modification = Int
type alias InstructionCode = Int
type alias Memory = Dict.Dict Address Word

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

type RuntimeError = NoMemoryValue Address
                  | InvalidModification Modification
                  | UnrecognizedInstructionCode (InstructionCode,Modification)
                  | InvalidIndex Index


type alias MixOperation a = Mix -> Result RuntimeError (Mix,a)


-- unpack instruction
type alias UnpackedWord = (Address,Modification,Masks,InstructionCode)
unpackInstruction : Mix -> MixOperation UnpackedWord
unpackInstruction m =
    case Dict.get m.p m.mem of
        Nothing                 -> throwError <| NoMemoryValue m.p
        Just (s,b1,b2,b3,b4,b5) -> let f v = return
                                             ( (smallWordValue (s,b1,b2) + v)
                                             , value b4
                                             , byteToMasks b4
                                             , value b5
                                             )
                                   in case value b3 of
                                          0 -> f 0 
                                          1 -> f <| smallWordValue <| m.i1
                                          2 -> f <| smallWordValue <| m.i2
                                          3 -> f <| smallWordValue <| m.i3
                                          4 -> f <| smallWordValue <| m.i4
                                          5 -> f <| smallWordValue <| m.i5
                                          6 -> f <| smallWordValue <| m.i6
                                          _ -> throwError <| InvalidIndex
                                                          <| value b3


-- increment program counter
incrementCounter : MixOperation () 
incrementCounter =
    let op m = { m | p = m.p + 1 }
    in (op <$> get) >>= put 

{-

design point:

we are not implementing multiplication and division at a machine level.
Instead, we have AddX and SubX.
Multiplication / Division are easily implemented as routines.

-}


type Instruction = LoadA Address Masks
                 | LoadX Address Masks
                 | LoadI1 Address Masks
                 | LoadI2 Address Masks
                 | LoadI3 Address Masks
                 | LoadI4 Address Masks
                 | LoadI5 Address Masks
                 | LoadI6 Address Masks
                 | LoadANeg Address Masks
                 | LoadXNeg Address Masks
                 | LoadI1Neg Address Masks
                 | LoadI2Neg Address Masks
                 | LoadI3Neg Address Masks
                 | LoadI4Neg Address Masks
                 | LoadI5Neg Address Masks
                 | LoadI6Neg Address Masks
                 | StoreA Address Masks
                 | StoreX Address Masks
                 | StoreI1 Address Masks
                 | StoreI2 Address Masks
                 | StoreI3 Address Masks
                 | StoreI4 Address Masks
                 | StoreI5 Address Masks
                 | StoreI6 Address Masks
                 | StoreJ Address Masks
                 | StoreZero Address Masks
                 | Add Address Masks
                 | Sub Address Masks
                 | AddX Masks
                 | SubX Masks
                 | EnterA Address
                 | EnterX Address
                 | EnterI1 Address
                 | EnterI2 Address
                 | EnterI3 Address
                 | EnterI4 Address
                 | EnterI5 Address
                 | EnterI6 Address
                 | EnterANeg Address
                 | EnterXNeg Address
                 | EnterI1Neg Address
                 | EnterI2Neg Address
                 | EnterI3Neg Address
                 | EnterI4Neg Address
                 | EnterI5Neg Address
                 | EnterI6Neg Address
                 | IncrementA Address
                 | IncrementX Address
                 | IncrementI1 Address
                 | IncrementI2 Address
                 | IncrementI3 Address
                 | IncrementI4 Address
                 | IncrementI5 Address
                 | IncrementI6 Address
                 | DecrementA Address
                 | DecrementX Address
                 | DecrementI1 Address
                 | DecrementI2 Address
                 | DecrementI3 Address
                 | DecrementI4 Address
                 | DecrementI5 Address
                 | DecrementI6 Address


{-

when adding a new instruction, you need to update
  decodeInstruction
  executeInstructionTransition

-}

decodeInstruction : UnpackedWord -> MixOperation Instruction
decodeInstruction (a,f,ms,c) =
    case (c,f) of
        (8,_)  -> return <| LoadA a ms
        (15,_) -> return <| LoadX a ms
        (9,_)  -> return <| LoadI1 a ms
        (10,_) -> return <| LoadI2 a ms
        (11,_) -> return <| LoadI3 a ms
        (12,_) -> return <| LoadI4 a ms
        (13,_) -> return <| LoadI5 a ms
        (14,_) -> return <| LoadI6 a ms
        (16,_) -> return <| LoadANeg a ms
        (23,_) -> return <| LoadXNeg a ms
        (17,_) -> return <| LoadI1Neg a ms
        (18,_) -> return <| LoadI2Neg a ms
        (19,_) -> return <| LoadI3Neg a ms
        (20,_) -> return <| LoadI4Neg a ms
        (21,_) -> return <| LoadI5Neg a ms
        (22,_) -> return <| LoadI6Neg a ms
        (24,_) -> return <| StoreA a ms
        (31,_) -> return <| StoreX a ms
        (25,_) -> return <| StoreI1 a ms
        (26,_) -> return <| StoreI2 a ms
        (27,_) -> return <| StoreI3 a ms
        (28,_) -> return <| StoreI4 a ms
        (29,_) -> return <| StoreI5 a ms
        (30,_) -> return <| StoreI6 a ms
        (32,_) -> return <| StoreJ a ms
        (33,_) -> return <| StoreZero a ms
        (1,_)  -> return <| Add a ms
        (2,_)  -> return <| Sub a ms
        (3,_)  -> return <| AddX ms
        (4,_)  -> return <| SubX ms
        (48,2) -> return <| EnterA a
        (55,2) -> return <| EnterX a
        (49,2) -> return <| EnterI1 a
        (50,2) -> return <| EnterI2 a
        (51,2) -> return <| EnterI3 a
        (52,2) -> return <| EnterI4 a
        (53,2) -> return <| EnterI5 a
        (54,2) -> return <| EnterI6 a
        (48,3) -> return <| EnterANeg a
        (55,3) -> return <| EnterXNeg a
        (49,3) -> return <| EnterI1Neg a
        (50,3) -> return <| EnterI2Neg a
        (51,3) -> return <| EnterI3Neg a
        (52,3) -> return <| EnterI4Neg a
        (53,3) -> return <| EnterI5Neg a
        (54,3) -> return <| EnterI6Neg a
        (48,0) -> return <| IncrementA a
        (55,0) -> return <| IncrementX a
        (49,0) -> return <| IncrementI1 a
        (50,0) -> return <| IncrementI2 a
        (51,0) -> return <| IncrementI3 a
        (52,0) -> return <| IncrementI4 a
        (53,0) -> return <| IncrementI5 a
        (54,0) -> return <| IncrementI6 a
        (48,1) -> return <| DecrementA a
        (55,1) -> return <| DecrementX a
        (49,1) -> return <| DecrementI1 a
        (50,1) -> return <| DecrementI2 a
        (51,1) -> return <| DecrementI3 a
        (52,1) -> return <| DecrementI4 a
        (53,1) -> return <| DecrementI5 a
        (54,1) -> return <| DecrementI6 a
        x  -> throwError <| UnrecognizedInstructionCode x




executeInstructionTransition : Instruction -> Mix -> Mix
executeInstructionTransition i s =
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




executeInstruction : Instruction -> MixOperation ()
executeInstruction i =
    ((executeInstructionTransition i) <$> get) >>= put


step : MixOperation () 
step =
    ((get >>= unpackInstruction) <* incrementCounter)
    >>= decodeInstruction
    >>= executeInstruction




-- monad operations

(>>=) : MixOperation a -> (a -> MixOperation b) -> MixOperation b
(>>=) p f = let q s = case p s of
                          Err err -> Err err
                          Ok (ss,x) -> f x ss
            in q

return : a -> MixOperation a
return x = let p s = Ok (s,x)
           in p

get : MixOperation Mix
get = let p s = Ok (s,s)
      in p

put : Mix -> MixOperation ()
put m = let p s = Ok (m,())
        in p

throwError : RuntimeError -> MixOperation a
throwError err = let p s = Err err
                 in p

(<*>) : MixOperation (a -> b) -> MixOperation a -> MixOperation b
(<*>) p q = let r s = case p s of
                          Err err -> Err err
                          Ok (ss,f) -> case q ss of
                                           Err err -> Err err
                                           Ok (sss,x) -> Ok (sss, f x)
            in r

                
(<$>) : ( a -> b ) -> MixOperation a -> MixOperation b
(<$>) f p = (return f) <*> p


map2 : ( a -> b -> c) -> MixOperation a -> MixOperation b -> MixOperation c
map2 f p q = ( f <$> p ) <*> q


(<*) : MixOperation a -> MixOperation b -> MixOperation a
(<*) p q = let f x y = x
           in map2 f p q

(*>) : MixOperation a -> MixOperation b -> MixOperation b
(*>) p q = let g x y = y
           in map2 g p q


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
