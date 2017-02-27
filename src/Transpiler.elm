{-

this code transpiles REL to ASM.
To do this, we build a symbol table to resolve names.

-}

module Transpiler exposing ( transpile
                           , SymbolTable
                           , TranspileError
                           )

import CodeFunctor exposing (..)
import Atom exposing (..)
import Instruction exposing (..)
import Tokenizer exposing (..)
import Dict

type alias SymbolTable = Dict.Dict String Int

type TranspileError = NonDeclaredLabel String

zipWithAddress : List REL ->
                   List Intermediate
zipWithAddress rs =
    let l = List.length rs
        ads = List.range 0 (l-1)
    in List.map2 f ads rs

f : Int -> REL -> Intermediate
f n r = mapCode (\l -> (l,n)) identity r

distrubuteNothing : (Maybe a,b) -> Maybe (a,b)
distrubuteNothing (l,n) =
    case l of
        Nothing -> Nothing
        Just x -> Just (x,n)


symbolTable : List Intermediate -> SymbolTable
symbolTable li =  Dict.fromList <| filterNothing
                  <| List.map ( distrubuteNothing << address) li


evaluateLabel : SymbolTable
              -> RelativeInstruction
              -> Result TranspileError StaticInstruction
evaluateLabel s (r,i,m,t) =
    case r of
        Value a -> Ok (a,i,m,t)
        Label l -> case Dict.get l s of
                       Nothing -> Err <| NonDeclaredLabel l
                       Just a -> Ok  (a,i,m,t)



intermediateToASM : SymbolTable -> Intermediate -> Result TranspileError ASM
intermediateToASM s i =
   distrubuteCodeError <| mapCode Tuple.second (evaluateLabel s) i

transpile : List REL -> Result TranspileError (List ASM,SymbolTable)
transpile l =
    let li = zipWithAddress l
        st = symbolTable li
    in Result.map (\l -> (l,st))
       <| distrubuteError <| List.map (intermediateToASM st) li
