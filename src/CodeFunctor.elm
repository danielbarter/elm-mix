{-

CodeFunctor is the internal representation of the assembly code which we get from the parser.

-}

module CodeFunctor exposing ( DataFunctor(..)
                            , CodeFunctor(..)
                            , ASM
                            , REL
                            , Intermediate
                            , address
                            , mapCode
                            , distrubuteCodeError
                            )

import Atom exposing (..)
import Instruction exposing (..)

type DataFunctor b = DataNumber Int
                   | DataInstruction b

type CodeFunctor a b = Code a (DataFunctor b)

mapCode : (a1 -> a2) -> (b1 -> b2) -> CodeFunctor a1 b1 -> CodeFunctor a2 b2
mapCode f g (Code x y) =
    case y of
        DataNumber n -> Code (f x) (DataNumber n)
        DataInstruction z -> Code (f x) (DataInstruction <| g z)

address : CodeFunctor a b -> a
address (Code x y) = x

distrubuteCodeError : CodeFunctor a (Result e b) -> Result e (CodeFunctor a b)
distrubuteCodeError (Code x y) =
    case y of
        DataNumber n -> Ok <| Code x (DataNumber n)
        DataInstruction z -> case z of
                                 Err err -> Err err
                                 Ok w -> Ok <| Code x (DataInstruction w)

type alias Label = String

type alias ASM = CodeFunctor Address StaticInstruction
type alias REL = CodeFunctor (Maybe Label) RelativeInstruction
type alias Intermediate = CodeFunctor (Maybe String,Address) RelativeInstruction
