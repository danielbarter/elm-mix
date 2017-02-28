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

type DataFunctor b c = DataNumber b
                     | DataInstruction c

type CodeFunctor a b c = Code a (DataFunctor b c)

mapCode : (a1 -> a2)
        -> (b1 -> b2)
        -> (c1 -> c2)
        -> CodeFunctor a1 b1 c1 -> CodeFunctor a2 b2 c2
mapCode f g h (Code x y) =
    case y of
        DataNumber n -> Code (f x) (DataNumber <| g n )
        DataInstruction z -> Code (f x) (DataInstruction <| h z)

address : CodeFunctor a b c -> a
address (Code x y) = x

distrubuteCodeError : CodeFunctor a (Result e b) (Result e c)
                    -> Result e (CodeFunctor a b c)
distrubuteCodeError (Code x y) =
    case y of
        DataNumber z -> case z of
                            Err err -> Err err
                            Ok n -> Ok <| Code x (DataNumber n)
        DataInstruction z -> case z of
                                 Err err -> Err err
                                 Ok w -> Ok <| Code x (DataInstruction w)

type alias Label = String

type alias ASM = CodeFunctor Address Int StaticInstruction
type alias REL = CodeFunctor (Maybe Label) RelativeAddress RelativeInstruction
type alias Intermediate =
    CodeFunctor (Maybe Label,Address) RelativeAddress RelativeInstruction
