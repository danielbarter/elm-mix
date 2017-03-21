{-

CodeFunctor is the internal representation of the assembly code which we get from the parser.

-}

module CodeFunctor exposing ( DataFunctor(..)
                            , DataValue(..)
                            , CodeFunctor(..)
                            , ASM
                            , REL
                            , Intermediate
                            , address
                            , mapCode
                            , distrubuteCodeError
                            , mapDataValue
                            , distrubuteDataValueError
                            , resolveDataValue
                            )

import Atom exposing (..)
import Instruction exposing (..)

resolveDataValue : DataValue Address -> Int
resolveDataValue v =
    case v of
        DataAddress n -> n
        DataPacked s l -> schemaPack s l

type DataValue a = DataAddress a
                 | DataPacked Schema (List a)

distrubuteDataValueError : DataValue (Result e a) -> Result e (DataValue a)
distrubuteDataValueError v =
    case v of
        DataAddress x
            -> case x of
                   Err err -> Err err
                   Ok y -> Ok <| DataAddress y
        DataPacked s l
            -> case distrubuteError l of
                   Err err -> Err err
                   Ok l -> Ok <| DataPacked s l

mapDataValue : (a -> b) -> (DataValue a) -> (DataValue b)
mapDataValue f v =
    case v of
        DataAddress x -> DataAddress <| f x
        DataPacked s l -> DataPacked s <| List.map f l

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
type alias REL =
    CodeFunctor (Maybe Label) (DataValue RelativeAddress) RelativeInstruction
type alias Intermediate =
    CodeFunctor (Maybe Label,Address) (DataValue RelativeAddress) RelativeInstruction
