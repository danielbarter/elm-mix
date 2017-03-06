module Model exposing (..)

import Atom exposing (..)
import Mix exposing (..)
import MixStep exposing (..)
import Compiler exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)

type alias Model = { sourceCode : String
                   , compileError : Maybe CompilerError
                   , mix : List Mix
                   , runtimeError : Maybe RuntimeError
                   }


type Msg = Compile
         | KillCurrentCore
         | StepForward
         | StepBackward
         | ReadCode String

model =
    { sourceCode = ""
    , compileError = Nothing
    , mix = []
    , runtimeError = Nothing
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        Compile
            -> case compile model.sourceCode of
                   Err e -> { model | compileError = Just e
                            , mix = []
                            }
                   Ok x -> { model | compileError = Nothing
                           , mix = [load x]
                           }
        KillCurrentCore
            -> { model | compileError = Nothing
               , mix = []
               , runtimeError = Nothing
               }
        StepForward
            -> case model.mix of
                   [] -> model
                   (m::ms) -> case MixStep.step m of
                                 Err e -> { model | runtimeError = Just e }
                                 Ok (mm,i)
                                     -> { model | mix = mm::(m::ms)
                                        , runtimeError = Nothing
                                        }
        StepBackward
            -> case model.mix of
                   [] -> model
                   (m::ms) -> { model | mix = ms
                              , runtimeError = Nothing
                              }
        ReadCode s -> { model | sourceCode = s }


view : Model -> Html Msg
view model =
    case model.mix of
        [] -> div []
              [ compileButton
              , sourceCodeBox
              , errorMessage model.compileError
              ]
        (m::ms) -> div []
                   [ runtimeButtons
                   , displayMix m
                   , errorMessage model.runtimeError
                   ]

runtimeButtons : Html Msg
runtimeButtons = div []
                 [ button [onClick StepForward] [text "step"]
                 , button [onClick StepBackward] [text "back"]
                 , button [onClick KillCurrentCore] [text "kill core"]
                 ]

compileButton : Html Msg
compileButton = div []
                [button [onClick Compile] [text "compile"]]

sourceCodeBox : Html Msg
sourceCodeBox =
    div []
    [ textarea [placeholder "Write source code here.", onInput ReadCode] []]

errorMessage : Maybe a -> Html Msg
errorMessage e =
    case e of
        Nothing -> text ""
        Just err -> text <| toString err


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

printColor : Color -> String
printColor c =
    let cc = toRgb c
        r = toString <| .red cc
        g = toString <| .green cc
        b = toString <| .blue cc
        a = toString <| .alpha cc
    in "rgba(" ++ (String.join "," [r,g,b,a]) ++ ")"


boxStyle : Color -> Color -> List (Attribute Msg)
boxStyle cb ct =
    [style [ ("background-color",printColor cb)
           , ("color",printColor ct)
           , ("display","inline-block")
           ]
    ]

displayMemData : Mix -> MemData -> Html Msg
displayMemData mix d =
    let (s,cb,ct) = ppMemData mix d
    in div
        (boxStyle cb ct)
        [text s]

displayWord : Word -> Html Msg
displayWord w =
    let (s,cb,ct) = ppWord w
    in div
        (boxStyle cb ct)
        [text s]


displaySmallWord : SmallWord -> Html Msg
displaySmallWord w =
    let (s,cb,ct) = ppSmallWord w
    in div
        (boxStyle cb ct)
        [text s]


displayOverflow : OverflowToggle -> Html Msg
displayOverflow t =
    let (s,cb,ct) = ppOverflow t
    in div
        (boxStyle cb ct)
        [text s]

displayComparison : ComparisonIndicator -> Html Msg
displayComparison t =
    let (s,cb,ct) = ppComparision t
    in div
        (boxStyle cb ct)
        [text s]


displayMem : Mix -> Html Msg
displayMem mix =
    let memDatas = totalMemData mix
    in div
        []
        (List.map (displayMemData mix) memDatas)

displayRegisters : Mix -> Html Msg
displayRegisters mix =
    div
    []
    [ displayWord mix.a
    , displayWord mix.x
    , displaySmallWord mix.i1
    , displaySmallWord mix.i2
    , displaySmallWord mix.i3
    , displaySmallWord mix.i4
    , displaySmallWord mix.i5
    , displaySmallWord mix.i6
    , displaySmallWord mix.j
    , displayOverflow mix.overflow
    , displayComparison mix.comparison
    ]

displayMix : Mix -> Html Msg
displayMix mix =
    div
    []
    [ displayMem mix
    , displayRegisters mix
    ]

