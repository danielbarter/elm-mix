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
                     )
{-

execution cycle:
  unpack instruction
  increment program counter
  decode instruction
  execute instruction

-}

type OverflowToggle = Overflow | Good
type ComparisonIndicator = L | E | G
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


type Instruction = LoadA Address Masks

decodeInstruction : UnpackedWord -> MixOperation Instruction
decodeInstruction (a,f,ms,c) =
    case c of
        8 -> return <| LoadA a ms
        x -> throwError <| UnrecognizedInstructionCode x




executeInstructionTransition : Instruction -> Mix -> Mix
executeInstructionTransition i s =
    case i of
        LoadA adr masks
            -> { s | a = copy masks (read adr s.mem) s.a }



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
testLoadA : Mix
testLoadA =
    let
        b = masksToByte (On,Off,On,Off,Off,Off)
        m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 0,b,byte 8))
                          , (2000,(Neg,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = zeroWord
       , x = zeroWord
       , i1 = zeroSmallWord
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
