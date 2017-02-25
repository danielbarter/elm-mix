module Model exposing ( Model
                      , Msg(..)
                      , update
                      , model
                      , view
                      )

import Assembler exposing (..)
import Mix exposing (..)
import MixStep exposing (..)
import Html

type alias Model =
    { sourceCode : String
    , compileError : Maybe CompileError
    , mixs : List Mix
    , runtimeError : Maybe RuntimeError
    }


type Msg = Compile
         | StepForward
         | StepBackward



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


model = { sourceCode = ""
        , compileError = Nothing
        , mixs = []
        , runtimeError = Nothing
        }

view : Model -> Html.Html Msg
view m = Html.text "Hello World!"
