module SpaceLeak exposing (..)



type Instruction = LoadA
                 | LoadX
                 | LoadI1
                 | LoadI2
                 | LoadI3
                 | LoadI4
                 | LoadI5
                 | LoadI6
                 | LoadANeg
                 | LoadXNeg
                 | LoadI1Neg
                 | LoadI2Neg
                 | LoadI3Neg
                 | LoadI4Neg
                 | LoadI5Neg
                 | LoadI6Neg
                 | StoreA
                 | StoreX
                 | StoreI1
                 | StoreI2
                 | StoreI3
                 | StoreI4
                 | StoreI5
                 | StoreI6
                 | StoreJ
                 | StoreZero
                 | Add
                 | Sub
                 | AddX
                 | SubX
                 | EnterA
                 | EnterX
                 | EnterI1
                 | EnterI2
                 | EnterI3
                 | EnterI4
                 | EnterI5
                 | EnterI6
                 | EnterANeg
                 | EnterXNeg
                 | EnterI1Neg
                 | EnterI2Neg
                 | EnterI3Neg
                 | EnterI4Neg
                 | EnterI5Neg
                 | EnterI6Neg
                 | IncrementA
                 | IncrementX
                 | IncrementI1
                 | IncrementI2
                 | IncrementI3
                 | IncrementI4
                 | IncrementI5
                 | IncrementI6
                 | DecrementA
                 | DecrementX
                 | DecrementI1
                 | DecrementI2
                 | DecrementI3
                 | DecrementI4
                 | DecrementI5
                 | DecrementI6
                 | UnrecognizedInstructionCode



decodeInstruction : (Int,Int) -> Instruction
decodeInstruction (f,c) =
    case (c,f) of
        (8,_)  -> LoadA 
        (15,_) -> LoadX
        (9,_)  -> LoadI1
        (10,_) -> LoadI2
        (11,_) -> LoadI3
        (12,_) -> LoadI4
        (13,_) -> LoadI5
        (14,_) -> LoadI6
        (16,_) -> LoadANeg
        (23,_) -> LoadXNeg
        (17,_) -> LoadI1Neg
        (18,_) -> LoadI2Neg
        (19,_) -> LoadI3Neg
        (20,_) -> LoadI4Neg
        (21,_) -> LoadI5Neg
        (22,_) -> LoadI6Neg
        (24,_) -> StoreA
        (31,_) -> StoreX
        (25,_) -> StoreI1
        (26,_) -> StoreI2
        (27,_) -> StoreI3
        (28,_) -> StoreI4
        (29,_) -> StoreI5
        (30,_) -> StoreI6
        (32,_) -> StoreJ
        (33,_) -> StoreZero
        (1,_)  -> Add
        (2,_)  -> Sub
        (3,_)  -> AddX
        (4,_)  -> SubX
        (48,2) -> EnterA
        (55,2) -> EnterX
        (49,2) -> EnterI1
        (50,2) -> EnterI2
        (51,2) -> EnterI3
        (52,2) -> EnterI4
        (53,2) -> EnterI5
        (54,2) -> EnterI6
        (48,3) -> EnterANeg
        (55,3) -> EnterXNeg
        (49,3) -> EnterI1Neg
        (50,3) -> EnterI2Neg
        (51,3) -> EnterI3Neg
        (52,3) -> EnterI4Neg
        (53,3) -> EnterI5Neg
        (54,3) -> EnterI6Neg
        (48,0) -> IncrementA
        (55,0) -> IncrementX
        (49,0) -> IncrementI1
        (50,0) -> IncrementI2
        (51,0) -> IncrementI3
        (52,0) -> IncrementI4
        (53,0) -> IncrementI5
        (54,0) -> IncrementI6
        (48,1) -> DecrementA
        (55,1) -> DecrementX
        (49,1) -> DecrementI1
        (50,1) -> DecrementI2
        (51,1) -> DecrementI3
        (52,1) -> DecrementI4
        (53,1) -> DecrementI5
        (54,1) -> DecrementI6
        x  -> UnrecognizedInstructionCode
