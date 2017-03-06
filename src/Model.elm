module Model exposing (..)

import Atom exposing (..)
import Instruction exposing (..)
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
                   , power : Power
                   }

type Power = On | Off

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
    , power = Off
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        Compile
            -> case compile model.sourceCode of
                   Err e -> { model | compileError = Just e
                            , mix = []
                            , power = Off
                            }
                   Ok x -> { model | compileError = Nothing
                           , mix = [load x]
                           , power = On
                           }
        KillCurrentCore
            -> { model | compileError = Nothing
               , mix = []
               , runtimeError = Nothing
               , power = Off
               }
        StepForward
            -> case model.mix of
                   [] -> model
                   (m::ms) -> case MixStep.step m of
                                 Err e -> { model | runtimeError = Just e }
                                 Ok (mm,i)
                                     -> case i of
                                            (_,_,Halt) -> { model | mix = mm::(m::ms)
                                                          , runtimeError = Nothing
                                                          , power = Off
                                                          }
                                            _ -> { model | mix = mm::(m::ms)
                                                 , runtimeError = Nothing
                                                 , power = On
                                                 }
        StepBackward
            -> case model.mix of
                   [] -> model
                   (m::ms) -> { model | mix = ms
                              , runtimeError = Nothing
                              , power = On
                              }
        ReadCode s -> { model | sourceCode = s }


view : Model -> Html Msg
view model =
    case model.mix of
        [] -> div []
              [ sourceCodeBox
              , compileButton
              , errorMessage model.compileError
              ]
        (m::ms) -> div []
                   [ sourceCodeBox
                   , runtimeButtons model.power
                   , displayMix m
                   , errorMessage model.runtimeError
                   ]

runtimeButtons : Power -> Html Msg
runtimeButtons p =
    case p of
        On -> div []
               [ button [onClick StepForward] [text "step"]
               , button [onClick StepBackward] [text "back"]
               , button [onClick KillCurrentCore] [text "kill core"]
               ]
        Off -> div []
                [ button [onClick StepBackward] [text "back"]
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
           , ("padding","10px")
           , ("border-radius","10px")
           , ("border-size","2px")
           , ("border-style","solid")
           , ("border-color","white")
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

displayJump : SmallWord -> Html Msg
displayJump w =
    let (s,cb,ct) = ppJump w
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
    , displayJump mix.j
    , displayOverflow mix.overflow
    , displayComparison mix.comparison
    ]

displayMix : Mix -> Html Msg
displayMix mix =
    div
    []
    [ displayRegisters mix
    , displayMem mix
    ]

