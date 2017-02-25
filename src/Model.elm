module Model exposing ( Model
                      , Msg(..)
                      , update
                      , model
                      , view
                      )

import Assembler exposing (..)
import Mix exposing (..)
import MixStep exposing (..)
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick,onInput)

type alias Model =
    { sourceCode : String
    , compileError : Maybe CompileError
    , mixs : List Mix
    , runtimeError : Maybe RuntimeError
    }


type Msg = Compile
         | StepForward
         | StepBackward
         | Store String



update : Msg -> Model -> Model
update message model =
    case message of
        Compile
            -> case compile model.sourceCode of
                   Err err -> { model
                              | compileError = Just err
                              }
                   Ok mems -> { model
                              | compileError = Nothing
                              , mixs = [load mems]
                              }
        StepForward
            -> case model.mixs of
                   [] -> model
                   (m::ms) -> case step m of
                                  Err err -> { model
                                             | mixs = m :: ms
                                             , runtimeError = Just err
                                             }
                                  Ok (mm,i) -> { model
                                               | mixs = mm :: (m :: ms)
                                               , runtimeError = Nothing
                                               }
        StepBackward
            -> case model.mixs of
                   [] -> model
                   (m :: ms) -> { model | mixs = ms }
        Store s -> { model | sourceCode = s }


model = { sourceCode = ""
        , compileError = Nothing
        , mixs = []
        , runtimeError = Nothing
        }

view : Model -> Html Msg
view m = div [] [ sourceCode
                , buttons
                ]

buttons : Html Msg
buttons = div []
          [ button [ onClick Compile ] [ text "compile" ]
          , button [ onClick StepForward ] [ text "step" ]
          , button [ onClick StepBackward ] [ text "back" ]
          ]

sourceCode : Html Msg
sourceCode = div []
             [ textarea [ placeholder "write source code here", onInput Store ] [] ]
