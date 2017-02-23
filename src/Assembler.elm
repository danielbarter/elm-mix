module Assembler exposing (..)

import Atom exposing (..)
import Instructions exposing (..)
import Mix exposing (..)

type ASM = ASMLit Int
         | ASMInst StaticInstruction

type alias Line = (Address,ASM)


disassembleWord : MetaMemory -> (Address, Word) -> Line
disassembleWord meta (a,w) =
    case readMeta a meta of
        Number -> (a,ASMLit <| wordValue w)
        Instruction -> case decodeInstruction <| unpack w of
                           Err err -> (a,ASMLit <| wordValue w)
                           Ok i -> (a,ASMInst i) 

type alias Start = Int
type alias End = Int

getWord : Memory -> Address -> (Address,Word)
getWord m a = (a,read a m)
    

disassemble : MetaMemory -> Memory -> Start -> End -> List Line
disassemble meta mem s e = List.map (disassembleWord meta)
                           <| List.map (getWord mem)
                           <| List.range s e

