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
                  | UnrecognizedInstructionCode InstructionCode
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

{-

when adding a new instruction, you need to update
  decodeInstruction
  executeInstructionTransition

-}
    
decodeInstruction : UnpackedWord -> MixOperation Instruction
decodeInstruction (a,f,ms,c) =
    case c of
        8  -> return <| LoadA a ms
        15 -> return <| LoadX a ms
        9  -> return <| LoadI1 a ms
        10 -> return <| LoadI2 a ms
        11 -> return <| LoadI3 a ms
        12 -> return <| LoadI4 a ms
        13 -> return <| LoadI5 a ms
        14 -> return <| LoadI6 a ms
        16 -> return <| LoadANeg a ms
        23 -> return <| LoadXNeg a ms
        17 -> return <| LoadI1Neg a ms
        18 -> return <| LoadI2Neg a ms
        19 -> return <| LoadI3Neg a ms
        20 -> return <| LoadI4Neg a ms
        21 -> return <| LoadI5Neg a ms
        22 -> return <| LoadI6Neg a ms
        24 -> return <| StoreA a ms
        31 -> return <| StoreX a ms
        25 -> return <| StoreI1 a ms
        26 -> return <| StoreI2 a ms
        27 -> return <| StoreI3 a ms
        28 -> return <| StoreI4 a ms
        29 -> return <| StoreI5 a ms
        30 -> return <| StoreI6 a ms
        32 -> return <| StoreJ a ms
        33 -> return <| StoreZero a ms
        1  -> return <| Add a ms
        2  -> return <| Sub a ms
        3  -> return <| AddX ms
        4  -> return <| SubX ms
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
