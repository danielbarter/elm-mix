module Model exposing ( Model
                      , Msg(..)
                      , update
                      , model
                      , view
                      )


import PrettyMix exposing (..)
import AssemblerASM exposing (..)
import Mix exposing (..)
import MixStep exposing (..)
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick,onInput)

type alias Model =
    { sourceCode : String
    , compileError : Maybe ASMCompileError
    , mixs : List Mix
    , runtimeError : Maybe RuntimeError
    }

ppModel : Model -> List String
ppModel m =
    case m.compileError of
        Just err -> [toString err]
        Nothing -> case m.runtimeError of
                       Just err -> [toString err]
                       Nothing -> case m.mixs of
                                      [] -> ["Compile a program!"]
                                      (x::xs) -> ppMix x


type Msg = Compile
         | StepForward
         | StepBackward
         | Store String



update : Msg -> Model -> Model
update message model =
    case message of
        Compile
            -> case compileASM model.sourceCode of
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
                   (m :: ms) -> { model
                                | mixs = ms
                                , runtimeError = Nothing
                                }
        Store s -> { model | sourceCode = s }


model = { sourceCode = ""
        , compileError = Nothing
        , mixs = []
        , runtimeError = Nothing
        }

view : Model -> Html Msg
view m = div [] ([ sourceCode
                , buttons
                ] ++ (List.map (div []
                                << List.singleton
                                << text) <| ppModel m))

buttons : Html Msg
buttons = div []
          [ button [ onClick Compile ] [ text "compile" ]
          , button [ onClick StepForward ] [ text "step" ]
          , button [ onClick StepBackward ] [ text "back" ]
          ]

sourceCode : Html Msg
sourceCode = div []
             [ textarea [ placeholder "Write source code here.", onInput Store ] [] ]
