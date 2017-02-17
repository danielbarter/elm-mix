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
                     )
{-

execution cycle:
  get instruction word
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


(<**>) : ( a -> b -> c) -> MixOperation a -> MixOperation b -> MixOperation c
(<**>) f p q = ( f <$> p ) <*> q

