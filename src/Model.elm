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
import MemData exposing (..)


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
view model = div []
             [ case model.mix of
                   [] -> div []
                         [ sourceCodeBox
                         , compileButton
                         , displayDefaultRegisters
                         , errorMessage model.compileError
                         ]
                   (m::ms) -> div []
                              [ sourceCodeBox
                              , runtimeButtons model.power
                              , displayMix m
                              , errorMessage model.runtimeError
                              ]
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
        Just err -> div (boxStyle darkRed white) [text <| toString err]



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
           , ("border-size","3px")
           , ("border-style","solid")
           , ("border-color","white")
           ]
    ]

outerBoxStyle : List (Attribute Msg)
outerBoxStyle =
    [ style [ ("display","inline-block")
            , ("border-size","3px")
            , ("border-style","solid")
            , ("border-color","white")
            ]
    ]

leftBoxStyle : Color -> Color -> List (Attribute Msg)
leftBoxStyle cb ct =
    [style [ ("background-color",printColor cb)
           , ("color",printColor ct)
           , ("display","inline-block")
           , ("padding","10px")
           , ("border-radius","10px 0px 0px 10px")
           ]
    ]


rightBoxStyle : Color -> Color -> List (Attribute Msg)
rightBoxStyle cb ct =
    [style [ ("background-color",printColor cb)
           , ("color",printColor ct)
           , ("display","inline-block")
           , ("padding","10px")
           , ("border-radius","0px 10px 10px 0px")
           ]
    ]

displayMemData : Mix -> MemData -> Html Msg
displayMemData mix d =
    let (p,mb,mt,s,cb,ct) = ppMemData mix d
    in div
        outerBoxStyle
        [ div (leftBoxStyle mb mt) [text p]
        , div (rightBoxStyle cb ct) [text s]
        ]

displayWord : Word -> Html Msg
displayWord w =
    let (s,cb,ct) = ppWord w
    in div
        (boxStyle cb ct)
        [text s]

displayDefaultWord : String -> Html Msg
displayDefaultWord s =
    let (_,cb,ct) = ppWord zeroWord
    in div
        (boxStyle cb ct)
        [text s]

displayDefaultSmallWord : String -> Html Msg
displayDefaultSmallWord s =
    let (_,cb,ct) = ppSmallWord zeroSmallWord
    in div
        (boxStyle cb ct)
        [text s]

displayDefaultJump : String -> Html Msg
displayDefaultJump s =
    let (_,cb,ct) = ppJump zeroSmallWord
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

displayDefaultOverflow : String -> Html Msg
displayDefaultOverflow s =
    let (_,cb,ct) = ppOverflow Good
    in div
        (boxStyle cb ct)
        [text s]

displayDefaultComparison : String -> Html Msg
displayDefaultComparison s =
    let (_,cb,ct) = ppComparision E
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

displayDefaultRegisters : Html Msg
displayDefaultRegisters =
    div
    []
    [ displayDefaultWord "A"
    , displayDefaultWord "X"
    , displayDefaultSmallWord "I1"
    , displayDefaultSmallWord "I2"
    , displayDefaultSmallWord "I3"
    , displayDefaultSmallWord "I4"
    , displayDefaultSmallWord "I5"
    , displayDefaultSmallWord "I6"
    , displayDefaultJump "J"
    , displayDefaultOverflow "Overflow"
    , displayDefaultComparison "Comparison"
    ]


displayMix : Mix -> Html Msg
displayMix mix =
    div
    []
    [ displayRegisters mix
    , displayMem mix
    ]
