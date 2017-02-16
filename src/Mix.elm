module Mix exposing (..)

import Atom exposing ( Sign(..)
                     , baseExpand
                     , baseContract
                     , Byte
                     , byte
                     , value
                     )

type alias RegisterA = ( Sign
                       , Byte
                       , Byte
                       , Byte
                       , Byte
                       , Byte
                       )

type alias RegisterX = ( Sign
                       , Byte
                       , Byte
                       , Byte
                       , Byte
                       , Byte
                       )

type alias RegisterI1 = ( Sign,Byte,Byte)
type alias RegisterI2 = ( Sign,Byte,Byte)
type alias RegisterI3 = ( Sign,Byte,Byte)
type alias RegisterI4 = ( Sign,Byte,Byte)
type alias RegisterI5 = ( Sign,Byte,Byte)
type alias RegisterI6 = ( Sign,Byte,Byte)

type alias RegisterIJ = ( Sign,Byte,Byte)
