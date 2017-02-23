module MixStep exposing ( step
                        , RuntimeError
                        , MixOperation
                        )

import Mix exposing (..)
import StateMonad exposing (..)
import Atom exposing (..)
import Instructions exposing (..)
import Dict

{-

execution cycle:
  decode instruction
  increment program counter
  relativise instruction
  execute instruction

-}


type RuntimeError = NoMemoryValue Address
                  | CompileError CompileTimeError
                  | InvalidIndex Index


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
        Err err -> throwError <| CompileError err
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
    

relativiseAddress : Mix -> (Index,Address) -> Result RuntimeError Address
relativiseAddress m (i,a) =
    case i of
        0 -> Ok a
        1 -> Ok <| ( smallWordValue m.i1 ) + a
        2 -> Ok <| ( smallWordValue m.i2 ) + a
        3 -> Ok <| ( smallWordValue m.i3 ) + a
        4 -> Ok <| ( smallWordValue m.i4 ) + a
        5 -> Ok <| ( smallWordValue m.i5 ) + a
        6 -> Ok <| ( smallWordValue m.i6 ) + a
        x -> Err <| InvalidIndex x

relativiseInstruction : Mix
                      -> StaticInstruction
                      -> MixOperation DynamicInstruction
relativiseInstruction m s =
    case distributeResult <| mapInstruction (relativiseAddress m) identity s of
        Err err -> throwError err
        Ok d -> return d

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
