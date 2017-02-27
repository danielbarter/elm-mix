{-

CodeFunctor is the internal representation of the assembly code which we get from the parser.

-}

module CodeFunctor exposing ( DataFunctor(..)
                            , CodeFunctor(..)
                            , ASM
                            , REL
                            )

import Atom exposing (..)
import Instruction exposing (..)

type DataFunctor b = DataNumber Int
                   | DataInstruction b

type CodeFunctor a b = Code a (DataFunctor b)

type alias Label = String

type alias ASM = CodeFunctor Address StaticInstruction
type alias REL = CodeFunctor (Maybe Label) RelativeInstruction
