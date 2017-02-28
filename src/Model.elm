module Model exposing (..)

import Mix exposing (..)
import MixStep exposing (..)
import Compiler exposing (..)

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

