module MixStep exposing (step)

import Mix exposing (..)
import MixOperation exposing (..)
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


nextWord : Mix -> MixOperation Mix Word
nextWord m =
    case Dict.get m.p m.mem of
        Nothing -> throwError <| NoMemoryValue m.p
        Just w  -> return w


unpackOp : Word -> MixOperation Mix UnpackedWord
unpackOp w = return <| unpack w

decodeOp : UnpackedWord -> MixOperation Mix StaticInstruction
decodeOp u =
    case decodeInstruction u of
        Err err -> throwError <| CompileError err
        Ok s -> return s

-- phase 1
decodeInstructionOp : MixOperation Mix StaticInstruction
decodeInstructionOp =
    get >>= nextWord >>= unpackOp >>= decodeOp


-- phase 2
incrementCounter : MixOperation Mix () 
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
                      -> MixOperation Mix DynamicInstruction
relativiseInstruction m s =
    case distributeResult <| mapInstruction (relativiseAddress m) s of
        Err err -> throwError err
        Ok d -> return d

-- phase 3
relativiseInstructionOp : StaticInstruction -> MixOperation Mix DynamicInstruction
relativiseInstructionOp s = get >>= (flip relativiseInstruction) s

-- phase 4
executeInstructionOp : DynamicInstruction -> MixOperation Mix DynamicInstruction
executeInstructionOp d = (((instructionTransition d) <$> get) >>= put) *> return d 

step : MixOperation Mix DynamicInstruction
step = (decodeInstructionOp <* incrementCounter)
       >>= relativiseInstructionOp
       >>= executeInstructionOp
