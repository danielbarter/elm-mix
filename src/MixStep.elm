module MixStep exposing ( step
                        , RuntimeError
                        , MixOperation
                        )

import Mix exposing (..)
import StateMonad exposing (..)
import Atom exposing (..)
import Instruction exposing (..)
import Dict


{-

execution cycle:
  decode instruction
  increment program counter
  relativise instruction
  execute instruction

-}


type RuntimeError = NoMemoryValue Address
                  | InstructionDecodeError DecodeError
                  | InvalidIndex Index
                  | CompileTimeError


type alias MixOperation a = State Mix RuntimeError a



nextWord : Mix -> MixOperation Word
nextWord m =
    case Dict.get m.p m.mem of
        Nothing -> throwError <| NoMemoryValue m.p
        Just w  -> return w


unpackOp : Word -> MixOperation UnpackedWord
unpackOp w = return <| unpack w

decodeOp : UnpackedWord -> MixOperation StaticInstruction
decodeOp u =
    case decodeInstruction u of
        Err err -> throwError <| InstructionDecodeError err
        Ok s -> return s

-- phase 1
decodeInstructionOp : MixOperation StaticInstruction
decodeInstructionOp =
    get >>= nextWord >>= unpackOp >>= decodeOp

-- phase 2
incrementCounter : MixOperation () 
incrementCounter =
    let op m = { m | p = m.p + 1 }
    in (op <$> get) >>= put


relativiseInstruction : Mix
                      -> StaticInstruction
                      -> MixOperation DynamicInstruction
relativiseInstruction m (a,i,ms,t) =
    case i of
        0 -> return (a,ms,t)
        1 -> return <| ( a + smallWordValue m.i1,ms,t)
        2 -> return <| ( a + smallWordValue m.i2,ms,t)
        3 -> return <| ( a + smallWordValue m.i3,ms,t)
        4 -> return <| ( a + smallWordValue m.i4,ms,t)
        5 -> return <| ( a + smallWordValue m.i5,ms,t)
        6 -> return <| ( a + smallWordValue m.i6,ms,t)
        x -> throwError <| InvalidIndex x

-- phase 3
relativiseInstructionOp : StaticInstruction -> MixOperation DynamicInstruction
relativiseInstructionOp s = get >>= (flip relativiseInstruction) s

-- phase 4
executeInstructionOp : DynamicInstruction -> MixOperation DynamicInstruction
executeInstructionOp d = (((instructionTransition d) <$> get) >>= put) *> return d 

step : MixOperation DynamicInstruction
step = (decodeInstructionOp <* incrementCounter)
       >>= relativiseInstructionOp
       >>= executeInstructionOp
