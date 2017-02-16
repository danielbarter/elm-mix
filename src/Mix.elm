module Mix exposing (..)

import Dict

import Atom exposing ( Sign(..)
                     , baseExpand
                     , baseContract
                     , Byte
                     , byte
                     , value
                     , mixBase
                     )


type alias SmallRegister = ( Sign, Byte, Byte )

type alias BigRegister = ( Sign
                         , Byte
                         , Byte
                         , Byte
                         , Byte
                         , Byte
                         )

type alias Memory = Dict.Dict Int BigRegister

type OverflowToggle = Overflow | Good
type ComparisonIndicator = L | E | G | None

type alias Mix = { a   : BigRegister
                 , x   : BigRegister
                 , i1  : SmallRegister
                 , i2  : SmallRegister
                 , i3  : SmallRegister
                 , i4  : SmallRegister
                 , i5  : SmallRegister
                 , i6  : SmallRegister
                 , j   : SmallRegister
                 , mem : Memory
                 , overflow : OverflowToggle
                 , comparision : ComparisonIndicator
                 }

    
type RuntimeError = NoMemoryValue Int


