module Model exposing (..)

import Mix exposing (..)
import MixStep exposing (..)
import Compiler exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model = { sourceCode : String
                   , compileError : Maybe CompilerError
                   , mix : List Mix
                   , runtimeError : Maybe RuntimeError
                   , mode : Mode
                   }

type Mode = Edit | Stop | Run

type Msg = Compile
         | KillCurrentCore
         | StepForward
         | StepBackward
         | ReadCode String

printColor : Color -> String
printColor c =
    let cc = toRgb c
        r = toString <| .red cc
        g = toString <| .green cc
        b = toString <| .blue cc
        a = toString <| .alpha cc
    in "rgba(" ++ (String.join "," [r,g,b,a]) ++ ")"

displayMemData : Mix -> MemData -> Html msg
displayMemData mix d =
    let (s,c) = ppMemData mix d
    in div
        [style [ ("background-color",printColor c)
               , ("color",printColor <| complement c)
               ]
        ]
        [text s]

