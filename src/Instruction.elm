module Instruction exposing (..)

import Atom exposing (..)

type alias StaticInstruction = (Address,Index,Masks,Tag)
type alias DynaicInstruction = (Address,Masks,Tag)

type Tag = LoadA                 -- LDA
         | LoadX                 -- LDX 
         | LoadI1                -- LD1 
         | LoadI2                -- LD2 
         | LoadI3                -- LD3 
         | LoadI4                -- LD4 
         | LoadI5                -- LD5 
         | LoadI6                -- LD6 
         | LoadANeg              -- LDAN 
         | LoadXNeg              -- LDXN 
         | LoadI1Neg             -- LD1N 
         | LoadI2Neg             -- LD2N 
         | LoadI3Neg             -- LD3N 
         | LoadI4Neg             -- LD4N 
         | LoadI5Neg             -- LD5N 
         | LoadI6Neg             -- LD6N 
         | StoreA                -- STA 
         | StoreX                -- STX 
         | StoreI1               -- ST1 
         | StoreI2               -- ST2      
         | StoreI3               -- ST3 
         | StoreI4               -- ST4 
         | StoreI5               -- ST5 
         | StoreI6               -- ST6 
         | StoreJ                -- STJ 
         | StoreZero             -- STZ 
         | Add                   -- ADD 
         | Sub                   -- SUB 
         | AddX                  -- ADDX
         | SubX                  -- SUBX
         | EnterA                -- ENTA 
         | EnterX                -- ENTX 
         | EnterI1               -- ENT1 
         | EnterI2               -- ENT2 
         | EnterI3               -- ENT3 
         | EnterI4               -- ENT4 
         | EnterI5               -- ENT5 
         | EnterI6               -- ENT6 
         | EnterANeg             -- ENNA 
         | EnterXNeg             -- ENNX 
         | EnterI1Neg            -- ENN1 
         | EnterI2Neg            -- ENN2 
         | EnterI3Neg            -- ENN3 
         | EnterI4Neg            -- ENN4 
         | EnterI5Neg            -- ENN5 
         | EnterI6Neg            -- ENN6 
         | IncrementA            -- INCA 
         | IncrementX            -- INCX 
         | IncrementI1           -- INC1 
         | IncrementI2           -- INC2 
         | IncrementI3           -- INC3 
         | IncrementI4           -- INC4 
         | IncrementI5           -- INC5 
         | IncrementI6           -- INC6 
         | DecrementA            -- DECA 
         | DecrementX            -- DECX 
         | DecrementI1           -- DEC1 
         | DecrementI2           -- DEC2 
         | DecrementI3           -- DEC3 
         | DecrementI4           -- DEC4 
         | DecrementI5           -- DEC5 
         | DecrementI6           -- DEC6 
         | CompareA              -- CMPA 
         | CompareX              -- CMPX 
         | CompareI1             -- CMP1 
         | CompareI2             -- CMP2 
         | CompareI3             -- CMP3 
         | CompareI4             -- CMP4 
         | CompareI5             -- CMP5 
         | CompareI6             -- CMP6 
         | Jump                  -- JMP 
         | JumpSaveJ             -- JSJ 
         | JumpOnOverflow        -- JOV 
         | JumpOnNoOverflow      -- JNOV 
         | JumpOnLess            -- JL 
         | JumpOnEqual           -- JE 
         | JumpOnGreater         -- JG 
         | JumpOnGreaterEqual    -- JGE 
         | JumpOnUnEqual         -- JNE 
         | JumpOnLessEqual       -- JLE 
         | JumpANegative         -- JAN 
         | JumpAZero             -- JAZ 
         | JumpAPositive         -- JAP 
         | JumpANonNegative      -- JANN 
         | JumpANonZero          -- JANZ 
         | JumpANonPositive      -- JANP 
         | JumpXNegative         -- JXN 
         | JumpXZero             -- JXZ 
         | JumpXPositive         -- JXP 
         | JumpXNonNegative      -- JXNN 
         | JumpXNonZero          -- JXNZ 
         | JumpXNonPositive      -- JXNP 
         | JumpI1Negative        -- J1N 
         | JumpI1Zero            -- J1Z 
         | JumpI1Positive        -- J1P 
         | JumpI1NonNegative     -- J1NN 
         | JumpI1NonZero         -- J1NZ 
         | JumpI1NonPositive     -- J1NP 
         | JumpI2Negative        -- J2N  
         | JumpI2Zero            -- J2Z 
         | JumpI2Positive        -- J2P 
         | JumpI2NonNegative     -- J2NN 
         | JumpI2NonZero         -- J2NZ 
         | JumpI2NonPositive     -- J2NP 
         | JumpI3Negative        -- J3N 
         | JumpI3Zero            -- J3Z 
         | JumpI3Positive        -- J3P 
         | JumpI3NonNegative     -- J3NN 
         | JumpI3NonZero         -- J3NZ 
         | JumpI3NonPositive     -- J3NP 
         | JumpI4Negative        -- J4N 
         | JumpI4Zero            -- J4Z 
         | JumpI4Positive        -- J4P 
         | JumpI4NonNegative     -- J4NN 
         | JumpI4NonZero         -- J4NZ 
         | JumpI4NonPositive     -- J4NP 
         | JumpI5Negative        -- J5N 
         | JumpI5Zero            -- J5Z 
         | JumpI5Positive        -- J5P 
         | JumpI5NonNegative     -- J5NN 
         | JumpI5NonZero         -- J5NZ 
         | JumpI5NonPositive     -- J5NP 
         | JumpI6Negative        -- J6N 
         | JumpI6Zero            -- J6Z 
         | JumpI6Positive        -- J6P 
         | JumpI6NonNegative     -- J6NN 
         | JumpI6NonZero         -- J6NZ 
         | JumpI6NonPositive     -- J6NP 
         | ShiftA                -- SA 
         | ShiftX                -- SX 
         | ShiftACircular        -- SAC 
         | ShiftXCircular        -- SAX 
         | SwapAX                -- SWAP
         | MoveXI1               -- MOVX1
         | MoveXI2               -- MOVX2
         | MoveXI3               -- MOVX3
         | MoveXI4               -- MOVX4
         | MoveXI5               -- MOVX5
         | MoveXI6               -- MOVX6
         | NoOperation           -- NOP
         | Halt                  -- HLT

type DecodeError = InvalidModification Modification
                 | UnrecognizedInstructionCode InstructionCode 
           
decodeInstruction : UnpackedWord -> Result DecodeError StaticInstruction
decodeInstruction (a,i,f,c) =
    let ms = byteToMasks <| byte f in
    case c of
        0  -> Ok (a,i,ms,NoOperation)
        8  -> Ok (a,i,ms,LoadA )
        15 -> Ok (a,i,ms,LoadX )
        9  -> Ok (a,i,ms,LoadI1 )
        10 -> Ok (a,i,ms,LoadI2 )
        11 -> Ok (a,i,ms,LoadI3 )
        12 -> Ok (a,i,ms,LoadI4 )
        13 -> Ok (a,i,ms,LoadI5 )
        14 -> Ok (a,i,ms,LoadI6 )
        16 -> Ok (a,i,ms,LoadANeg )
        23 -> Ok (a,i,ms,LoadXNeg )
        17 -> Ok (a,i,ms,LoadI1Neg )
        18 -> Ok (a,i,ms,LoadI2Neg )
        19 -> Ok (a,i,ms,LoadI3Neg )
        20 -> Ok (a,i,ms,LoadI4Neg )
        21 -> Ok (a,i,ms,LoadI5Neg )
        22 -> Ok (a,i,ms,LoadI6Neg )
        24 -> Ok (a,i,ms,StoreA )
        31 -> Ok (a,i,ms,StoreX )
        25 -> Ok (a,i,ms,StoreI1 )
        26 -> Ok (a,i,ms,StoreI2 )
        27 -> Ok (a,i,ms,StoreI3 )
        28 -> Ok (a,i,ms,StoreI4 )
        29 -> Ok (a,i,ms,StoreI5 )
        30 -> Ok (a,i,ms,StoreI6 )
        32 -> Ok (a,i,ms,StoreJ )
        33 -> Ok (a,i,ms,StoreZero )
        1  -> Ok (a,i,ms,Add )
        2  -> Ok (a,i,ms,Sub )
        3  -> Ok (a,i,ms,AddX )
        4  -> Ok (a,i,ms,SubX )
        56 -> Ok (a,i,ms,CompareA )
        63 -> Ok (a,i,ms,CompareX )
        57 -> Ok (a,i,ms,CompareI1 )
        58 -> Ok (a,i,ms,CompareI2 )
        59 -> Ok (a,i,ms,CompareI3 )
        60 -> Ok (a,i,ms,CompareI4 )
        61 -> Ok (a,i,ms,CompareI5 )
        62 -> Ok (a,i,ms,CompareI6 )
        48 -> case f of
                  2 -> Ok (a,i,ms,EnterA )
                  3 -> Ok (a,i,ms,EnterANeg )
                  0 -> Ok (a,i,ms,IncrementA )
                  1 -> Ok (a,i,ms,DecrementA )
                  y -> Err <| InvalidModification f
        55 -> case f of
                  2 -> Ok (a,i,ms,EnterX )
                  3 -> Ok (a,i,ms,EnterXNeg )
                  0 -> Ok (a,i,ms,IncrementX )
                  1 -> Ok (a,i,ms,DecrementX )
                  y -> Err <| InvalidModification f
        49 -> case f of
                  2 -> Ok (a,i,ms,EnterI1 )
                  3 -> Ok (a,i,ms,EnterI1Neg )
                  0 -> Ok (a,i,ms,IncrementI1 )
                  1 -> Ok (a,i,ms,DecrementI1 )
                  y -> Err <| InvalidModification f
        50 -> case f of
                  2 -> Ok (a,i,ms,EnterI2 )
                  3 -> Ok (a,i,ms,EnterI2Neg )
                  0 -> Ok (a,i,ms,IncrementI2 )
                  1 -> Ok (a,i,ms,DecrementI2 )
                  y -> Err <| InvalidModification f
        51 -> case f of
                  2 -> Ok (a,i,ms,EnterI3 )
                  3 -> Ok (a,i,ms,EnterI3Neg )
                  0 -> Ok (a,i,ms,IncrementI3 )
                  1 -> Ok (a,i,ms,DecrementI3 )
                  y -> Err <| InvalidModification f
        52 -> case f of
                  2 -> Ok (a,i,ms,EnterI4 )
                  3 -> Ok (a,i,ms,EnterI4Neg )
                  0 -> Ok (a,i,ms,IncrementI4 )
                  1 -> Ok (a,i,ms,DecrementI4 )
                  y -> Err <| InvalidModification f
        53 -> case f of
                  2 -> Ok (a,i,ms,EnterI5 )
                  3 -> Ok (a,i,ms,EnterI5Neg )
                  0 -> Ok (a,i,ms,IncrementI5 )
                  1 -> Ok (a,i,ms,DecrementI5 )
                  y -> Err <| InvalidModification f
        54 -> case f of
                  2 -> Ok (a,i,ms,EnterI6 )
                  3 -> Ok (a,i,ms,EnterI6Neg )
                  0 -> Ok (a,i,ms,IncrementI6 )
                  1 -> Ok (a,i,ms,DecrementI6 )
                  y -> Err <| InvalidModification f
        39 -> case f of
                  0 -> Ok (a,i,ms,Jump )
                  1 -> Ok (a,i,ms,JumpSaveJ )
                  2 -> Ok (a,i,ms,JumpOnOverflow )
                  3 -> Ok (a,i,ms,JumpOnNoOverflow )
                  4 -> Ok (a,i,ms,JumpOnLess )
                  5 -> Ok (a,i,ms,JumpOnEqual )
                  6 -> Ok (a,i,ms,JumpOnGreater )
                  7 -> Ok (a,i,ms,JumpOnGreaterEqual )
                  8 -> Ok (a,i,ms,JumpOnUnEqual )
                  9 -> Ok (a,i,ms,JumpOnLessEqual )
                  y -> Err <| InvalidModification f
        40 -> case f of
                  0 -> Ok (a,i,ms,JumpANegative )
                  1 -> Ok (a,i,ms,JumpAZero )
                  2 -> Ok (a,i,ms,JumpAPositive )
                  3 -> Ok (a,i,ms,JumpANonNegative )
                  4 -> Ok (a,i,ms,JumpANonZero )
                  5 -> Ok (a,i,ms,JumpANonPositive )
                  y -> Err <| InvalidModification f
        47 -> case f of
                  0 -> Ok (a,i,ms,JumpXNegative )
                  1 -> Ok (a,i,ms,JumpXZero )
                  2 -> Ok (a,i,ms,JumpXPositive )
                  3 -> Ok (a,i,ms,JumpXNonNegative )
                  4 -> Ok (a,i,ms,JumpXNonZero )
                  5 -> Ok (a,i,ms,JumpXNonPositive )
                  y -> Err <| InvalidModification f
        41 -> case f of
                  0 -> Ok (a,i,ms,JumpI1Negative )
                  1 -> Ok (a,i,ms,JumpI1Zero )
                  2 -> Ok (a,i,ms,JumpI1Positive )
                  3 -> Ok (a,i,ms,JumpI1NonNegative )
                  4 -> Ok (a,i,ms,JumpI1NonZero )
                  5 -> Ok (a,i,ms,JumpI1NonPositive )
                  y -> Err <| InvalidModification f
        42 -> case f of
                  0 -> Ok (a,i,ms,JumpI2Negative )
                  1 -> Ok (a,i,ms,JumpI2Zero )
                  2 -> Ok (a,i,ms,JumpI2Positive )
                  3 -> Ok (a,i,ms,JumpI2NonNegative )
                  4 -> Ok (a,i,ms,JumpI2NonZero )
                  5 -> Ok (a,i,ms,JumpI2NonPositive )
                  y -> Err <| InvalidModification f
        43 -> case f of
                  0 -> Ok (a,i,ms,JumpI3Negative )
                  1 -> Ok (a,i,ms,JumpI3Zero )
                  2 -> Ok (a,i,ms,JumpI3Positive )
                  3 -> Ok (a,i,ms,JumpI3NonNegative )
                  4 -> Ok (a,i,ms,JumpI3NonZero )
                  5 -> Ok (a,i,ms,JumpI3NonPositive )
                  y -> Err <| InvalidModification f
        44 -> case f of
                  0 -> Ok (a,i,ms,JumpI4Negative )
                  1 -> Ok (a,i,ms,JumpI4Zero )
                  2 -> Ok (a,i,ms,JumpI4Positive )
                  3 -> Ok (a,i,ms,JumpI4NonNegative )
                  4 -> Ok (a,i,ms,JumpI4NonZero )
                  5 -> Ok (a,i,ms,JumpI4NonPositive )
                  y -> Err <| InvalidModification f
        45 -> case f of
                  0 -> Ok (a,i,ms,JumpI5Negative )
                  1 -> Ok (a,i,ms,JumpI5Zero )
                  2 -> Ok (a,i,ms,JumpI5Positive )
                  3 -> Ok (a,i,ms,JumpI5NonNegative )
                  4 -> Ok (a,i,ms,JumpI5NonZero )
                  5 -> Ok (a,i,ms,JumpI5NonPositive )
                  y -> Err <| InvalidModification f
        46 -> case f of
                  0 -> Ok (a,i,ms,JumpI6Negative )
                  1 -> Ok (a,i,ms,JumpI6Zero )
                  2 -> Ok (a,i,ms,JumpI6Positive )
                  3 -> Ok (a,i,ms,JumpI6NonNegative )
                  4 -> Ok (a,i,ms,JumpI6NonZero )
                  5 -> Ok (a,i,ms,JumpI6NonPositive )
                  y -> Err <| InvalidModification f
        6  -> case f of
                  0 -> Ok (a,i,ms,ShiftA )
                  1 -> Ok (a,i,ms,ShiftX )
                  2 -> Ok (a,i,ms,ShiftACircular )
                  3 -> Ok (a,i,ms,ShiftXCircular )
                  4 -> Ok (a,i,ms,SwapAX)
                  y -> Err <| InvalidModification f
        7  -> case f of
                  0 -> Ok (a,i,ms,MoveXI1)
                  1 -> Ok (a,i,ms,MoveXI2)
                  2 -> Ok (a,i,ms,MoveXI3)
                  3 -> Ok (a,i,ms,MoveXI4)
                  4 -> Ok (a,i,ms,MoveXI5)
                  5 -> Ok (a,i,ms,MoveXI6)
                  y -> Err <| InvalidModification f
        5  -> case f of
                  2 -> Ok (a,i,ms,Halt)
                  y -> Err <| InvalidModification f
        x  -> Err <| UnrecognizedInstructionCode x

