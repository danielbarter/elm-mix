{-

The instruction data type.
It is used to organize the machine state transition function

-}

module Instruction exposing ( Tag(..)
                            , RelativeAddress(..)
                            , StaticInstruction
                            , RelativeInstruction
                            , DynamicInstruction
                            , DecodeError
                            , decodeInstruction
                            , encodeInstruction
                            , ppTag
                            )

import Atom exposing (..)
import Color exposing (..)

type RelativeAddress = Label String
                     | Value Address

type alias StaticInstructionClean = (Maybe Address, Maybe Index, Maybe Masks, Tag)
type alias StaticInstruction   = (Address,Index,Masks,Tag)
type alias RelativeInstruction = (RelativeAddress,Index,Masks,Tag)
type alias DynamicInstruction  = (Address,Masks,Tag)



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


ppTag : Tag -> (String,Color,Color)
ppTag t =
    case t of
       LoadA                -> ( "LDA",lightRed,darkRed)
       LoadX                -> ( "LDX",lightRed,darkRed)
       LoadI1               -> ( "LD1",lightRed,darkRed)
       LoadI2               -> ( "LD2",lightRed,darkRed)
       LoadI3               -> ( "LD3",lightRed,darkRed)
       LoadI4               -> ( "LD4",lightRed,darkRed)
       LoadI5               -> ( "LD5",lightRed,darkRed)
       LoadI6               -> ( "LD6",lightRed,darkRed)
       LoadANeg             -> ( "LDAN",lightRed,darkRed)
       LoadXNeg             -> ( "LDXN",lightRed,darkRed)
       LoadI1Neg            -> ( "LD1N",lightRed,darkRed)
       LoadI2Neg            -> ( "LD2N",lightRed,darkRed)
       LoadI3Neg            -> ( "LD3N",lightRed,darkRed)
       LoadI4Neg            -> ( "LD4N",lightRed,darkRed)
       LoadI5Neg            -> ( "LD5N",lightRed,darkRed)
       LoadI6Neg            -> ( "LD6N",lightRed,darkRed)
       StoreA               -> ( "STA",lightBlue,darkBlue)
       StoreX               -> ( "STX",lightBlue,darkBlue)
       StoreI1              -> ( "ST1",lightBlue,darkBlue)
       StoreI2              -> ( "ST2",lightBlue,darkBlue)
       StoreI3              -> ( "ST3",lightBlue,darkBlue)
       StoreI4              -> ( "ST4",lightBlue,darkBlue)
       StoreI5              -> ( "ST5",lightBlue,darkBlue)
       StoreI6              -> ( "ST6",lightBlue,darkBlue)
       StoreJ               -> ( "STJ",lightBlue,darkBlue)
       StoreZero            -> ( "STZ",lightBlue,darkBlue)
       Add                  -> ( "ADD",lightGreen,darkGreen)
       Sub                  -> ( "SUB",lightGreen,darkGreen)
       AddX                 -> ( "ADDX",lightGreen,darkGreen)
       SubX                 -> ( "SUBX",lightGreen,darkGreen)
       EnterA               -> ( "ENTA",lightOrange,darkOrange)
       EnterX               -> ( "ENTX",lightOrange,darkOrange)
       EnterI1              -> ( "ENT1",lightOrange,darkOrange)
       EnterI2              -> ( "ENT2",lightOrange,darkOrange)
       EnterI3              -> ( "ENT3",lightOrange,darkOrange)
       EnterI4              -> ( "ENT4",lightOrange,darkOrange)
       EnterI5              -> ( "ENT5",lightOrange,darkOrange)
       EnterI6              -> ( "ENT6",lightOrange,darkOrange)
       EnterANeg            -> ( "ENNA",lightOrange,darkOrange)
       EnterXNeg            -> ( "ENNX",lightOrange,darkOrange)
       EnterI1Neg           -> ( "ENN1",lightOrange,darkOrange)
       EnterI2Neg           -> ( "ENN2",lightOrange,darkOrange)
       EnterI3Neg           -> ( "ENN3",lightOrange,darkOrange)
       EnterI4Neg           -> ( "ENN4",lightOrange,darkOrange)
       EnterI5Neg           -> ( "ENN5",lightOrange,darkOrange)
       EnterI6Neg           -> ( "ENN6",lightOrange,darkOrange)
       IncrementA           -> ( "INCA",lightGreen,darkGreen)
       IncrementX           -> ( "INCX",lightGreen,darkGreen)
       IncrementI1          -> ( "INC1",lightGreen,darkGreen)
       IncrementI2          -> ( "INC2",lightGreen,darkGreen)
       IncrementI3          -> ( "INC3",lightGreen,darkGreen)
       IncrementI4          -> ( "INC4",lightGreen,darkGreen)
       IncrementI5          -> ( "INC5",lightGreen,darkGreen)
       IncrementI6          -> ( "INC6",lightGreen,darkGreen)
       DecrementA           -> ( "DECA",lightGreen,darkGreen)
       DecrementX           -> ( "DECX",lightGreen,darkGreen)
       DecrementI1          -> ( "DEC1",lightGreen,darkGreen)
       DecrementI2          -> ( "DEC2",lightGreen,darkGreen)
       DecrementI3          -> ( "DEC3",lightGreen,darkGreen)
       DecrementI4          -> ( "DEC4",lightGreen,darkGreen)
       DecrementI5          -> ( "DEC5",lightGreen,darkGreen)
       DecrementI6          -> ( "DEC6",lightGreen,darkGreen)
       CompareA             -> ( "CMPA",lightYellow,darkYellow)
       CompareX             -> ( "CMPX",lightYellow,darkYellow)
       CompareI1            -> ( "CMP1",lightYellow,darkYellow)
       CompareI2            -> ( "CMP2",lightYellow,darkYellow)
       CompareI3            -> ( "CMP3",lightYellow,darkYellow)
       CompareI4            -> ( "CMP4",lightYellow,darkYellow)
       CompareI5            -> ( "CMP5",lightYellow,darkYellow)
       CompareI6            -> ( "CMP6",lightYellow,darkYellow)
       Jump                 -> ( "JMP",lightPurple,darkPurple)
       JumpSaveJ            -> ( "JSJ",lightPurple,darkPurple)
       JumpOnOverflow       -> ( "JOV",lightPurple,darkPurple)
       JumpOnNoOverflow     -> ( "JNOV",lightPurple,darkPurple)
       JumpOnLess           -> ( "JL",lightPurple,darkPurple)
       JumpOnEqual          -> ( "JE",lightPurple,darkPurple)
       JumpOnGreater        -> ( "JG",lightPurple,darkPurple)
       JumpOnGreaterEqual   -> ( "JGE",lightPurple,darkPurple)
       JumpOnUnEqual        -> ( "JNE",lightPurple,darkPurple)
       JumpOnLessEqual      -> ( "JLE",lightPurple,darkPurple)
       JumpANegative        -> ( "JAN",lightPurple,darkPurple)
       JumpAZero            -> ( "JAZ",lightPurple,darkPurple)
       JumpAPositive        -> ( "JAP",lightPurple,darkPurple)
       JumpANonNegative     -> ( "JANN",lightPurple,darkPurple)
       JumpANonZero         -> ( "JANZ",lightPurple,darkPurple)
       JumpANonPositive     -> ( "JANP",lightPurple,darkPurple)
       JumpXNegative        -> ( "JXN",lightPurple,darkPurple)
       JumpXZero            -> ( "JXZ",lightPurple,darkPurple)
       JumpXPositive        -> ( "JXP",lightPurple,darkPurple)
       JumpXNonNegative     -> ( "JXNN",lightPurple,darkPurple)
       JumpXNonZero         -> ( "JXNZ",lightPurple,darkPurple)
       JumpXNonPositive     -> ( "JXNP",lightPurple,darkPurple)
       JumpI1Negative       -> ( "J1N",lightPurple,darkPurple)
       JumpI1Zero           -> ( "J1Z",lightPurple,darkPurple)
       JumpI1Positive       -> ( "J1P",lightPurple,darkPurple)
       JumpI1NonNegative    -> ( "J1NN",lightPurple,darkPurple)
       JumpI1NonZero        -> ( "J1NZ",lightPurple,darkPurple)
       JumpI1NonPositive    -> ( "J1NP",lightPurple,darkPurple)
       JumpI2Negative       -> ( "J2N",lightPurple,darkPurple)
       JumpI2Zero           -> ( "J2Z",lightPurple,darkPurple)
       JumpI2Positive       -> ( "J2P",lightPurple,darkPurple)
       JumpI2NonNegative    -> ( "J2NN",lightPurple,darkPurple)
       JumpI2NonZero        -> ( "J2NZ",lightPurple,darkPurple)
       JumpI2NonPositive    -> ( "J2NP",lightPurple,darkPurple)
       JumpI3Negative       -> ( "J3N",lightPurple,darkPurple)
       JumpI3Zero           -> ( "J3Z",lightPurple,darkPurple)
       JumpI3Positive       -> ( "J3P",lightPurple,darkPurple)
       JumpI3NonNegative    -> ( "J3NN",lightPurple,darkPurple)
       JumpI3NonZero        -> ( "J3NZ",lightPurple,darkPurple)
       JumpI3NonPositive    -> ( "J3NP",lightPurple,darkPurple)
       JumpI4Negative       -> ( "J4N",lightPurple,darkPurple)
       JumpI4Zero           -> ( "J4Z",lightPurple,darkPurple)
       JumpI4Positive       -> ( "J4P",lightPurple,darkPurple)
       JumpI4NonNegative    -> ( "J4NN",lightPurple,darkPurple)
       JumpI4NonZero        -> ( "J4NZ",lightPurple,darkPurple)
       JumpI4NonPositive    -> ( "J4NP",lightPurple,darkPurple)
       JumpI5Negative       -> ( "J5N",lightPurple,darkPurple)
       JumpI5Zero           -> ( "J5Z",lightPurple,darkPurple)
       JumpI5Positive       -> ( "J5P",lightPurple,darkPurple)
       JumpI5NonNegative    -> ( "J5NN",lightPurple,darkPurple)
       JumpI5NonZero        -> ( "J5NZ",lightPurple,darkPurple)
       JumpI5NonPositive    -> ( "J5NP",lightPurple,darkPurple)
       JumpI6Negative       -> ( "J6N",lightPurple,darkPurple)
       JumpI6Zero           -> ( "J6Z",lightPurple,darkPurple)
       JumpI6Positive       -> ( "J6P",lightPurple,darkPurple)
       JumpI6NonNegative    -> ( "J6NN",lightPurple,darkPurple)
       JumpI6NonZero        -> ( "J6NZ",lightPurple,darkPurple)
       JumpI6NonPositive    -> ( "J6NP",lightPurple,darkPurple)
       ShiftA               -> ( "SA",lightGreen,darkGreen)
       ShiftX               -> ( "SX",lightGreen,darkGreen)
       ShiftACircular       -> ( "SAC",lightGreen,darkGreen)
       ShiftXCircular       -> ( "SAX",lightGreen,darkGreen)
       SwapAX               -> ( "SWAP",lightGreen,darkGreen)
       MoveXI1              -> ( "MOVX1",lightGreen,darkGreen)
       MoveXI2              -> ( "MOVX2",lightGreen,darkGreen)
       MoveXI3              -> ( "MOVX3",lightGreen,darkGreen)
       MoveXI4              -> ( "MOVX4",lightGreen,darkGreen)
       MoveXI5              -> ( "MOVX5",lightGreen,darkGreen)
       MoveXI6              -> ( "MOVX6",lightGreen,darkGreen)
       NoOperation          -> ( "NOP",lightBrown,darkBrown)
       Halt                 -> ( "HLT",lightBrown,darkBrown)



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


encodeInstruction : StaticInstruction -> UnpackedWord
encodeInstruction (a,i,ms,inst) =
    let f = value <| masksToByte ms in
    case inst of
       LoadA                -> (a,i,f,8)
       LoadX                -> (a,i,f,15)
       LoadI1               -> (a,i,f,9)
       LoadI2               -> (a,i,f,10)
       LoadI3               -> (a,i,f,11)
       LoadI4               -> (a,i,f,12)
       LoadI5               -> (a,i,f,13)
       LoadI6               -> (a,i,f,14)
       LoadANeg             -> (a,i,f,16)
       LoadXNeg             -> (a,i,f,23)
       LoadI1Neg            -> (a,i,f,17)
       LoadI2Neg            -> (a,i,f,18)
       LoadI3Neg            -> (a,i,f,19)
       LoadI4Neg            -> (a,i,f,20)
       LoadI5Neg            -> (a,i,f,21)
       LoadI6Neg            -> (a,i,f,22)
       StoreA               -> (a,i,f,24)
       StoreX               -> (a,i,f,31)
       StoreI1              -> (a,i,f,25)
       StoreI2              -> (a,i,f,26)
       StoreI3              -> (a,i,f,27)
       StoreI4              -> (a,i,f,28)
       StoreI5              -> (a,i,f,29)
       StoreI6              -> (a,i,f,30)
       StoreJ               -> (a,i,f,32)
       StoreZero            -> (a,i,f,33)
       Add                  -> (a,i,f,1)
       Sub                  -> (a,i,f,2)
       AddX                 -> (a,i,f,3)
       SubX                 -> (a,i,f,4)
       EnterA               -> (a,i,2,48)
       EnterX               -> (a,i,2,55)
       EnterI1              -> (a,i,2,49)
       EnterI2              -> (a,i,2,50)
       EnterI3              -> (a,i,2,51)
       EnterI4              -> (a,i,2,52)
       EnterI5              -> (a,i,2,53)
       EnterI6              -> (a,i,2,54)
       EnterANeg            -> (a,i,3,48)
       EnterXNeg            -> (a,i,3,55)
       EnterI1Neg           -> (a,i,3,49)
       EnterI2Neg           -> (a,i,3,50)
       EnterI3Neg           -> (a,i,3,51)
       EnterI4Neg           -> (a,i,3,52)
       EnterI5Neg           -> (a,i,3,53)
       EnterI6Neg           -> (a,i,3,54)
       IncrementA           -> (a,i,0,48)
       IncrementX           -> (a,i,0,55)
       IncrementI1          -> (a,i,0,49)
       IncrementI2          -> (a,i,0,50)
       IncrementI3          -> (a,i,0,51)
       IncrementI4          -> (a,i,0,52)
       IncrementI5          -> (a,i,0,53)
       IncrementI6          -> (a,i,0,54)
       DecrementA           -> (a,i,1,48)
       DecrementX           -> (a,i,1,55)
       DecrementI1          -> (a,i,1,49)
       DecrementI2          -> (a,i,1,50)
       DecrementI3          -> (a,i,1,51)
       DecrementI4          -> (a,i,1,52)
       DecrementI5          -> (a,i,1,53)
       DecrementI6          -> (a,i,1,54)
       CompareA             -> (a,i,f,56)
       CompareX             -> (a,i,f,63)
       CompareI1            -> (a,i,f,57)
       CompareI2            -> (a,i,f,58)
       CompareI3            -> (a,i,f,59)
       CompareI4            -> (a,i,f,60)
       CompareI5            -> (a,i,f,61)
       CompareI6            -> (a,i,f,62)
       Jump                 -> (a,i,0,39)
       JumpSaveJ            -> (a,i,1,39)
       JumpOnOverflow       -> (a,i,2,39)
       JumpOnNoOverflow     -> (a,i,3,39)
       JumpOnLess           -> (a,i,4,39)
       JumpOnEqual          -> (a,i,5,39)
       JumpOnGreater        -> (a,i,6,39)
       JumpOnGreaterEqual   -> (a,i,7,39)
       JumpOnUnEqual        -> (a,i,8,39)
       JumpOnLessEqual      -> (a,i,9,39)
       JumpANegative        -> (a,i,0,40)
       JumpAZero            -> (a,i,1,40)
       JumpAPositive        -> (a,i,2,40)
       JumpANonNegative     -> (a,i,3,40)
       JumpANonZero         -> (a,i,4,40)
       JumpANonPositive     -> (a,i,5,40)
       JumpXNegative        -> (a,i,0,47)
       JumpXZero            -> (a,i,1,47)
       JumpXPositive        -> (a,i,2,47)
       JumpXNonNegative     -> (a,i,3,47)
       JumpXNonZero         -> (a,i,4,47)
       JumpXNonPositive     -> (a,i,5,47)
       JumpI1Negative       -> (a,i,0,41)
       JumpI1Zero           -> (a,i,1,41)
       JumpI1Positive       -> (a,i,2,41)
       JumpI1NonNegative    -> (a,i,3,41)
       JumpI1NonZero        -> (a,i,4,41)
       JumpI1NonPositive    -> (a,i,5,41)
       JumpI2Negative       -> (a,i,0,42)
       JumpI2Zero           -> (a,i,1,42)
       JumpI2Positive       -> (a,i,2,42)
       JumpI2NonNegative    -> (a,i,3,42)
       JumpI2NonZero        -> (a,i,4,42)
       JumpI2NonPositive    -> (a,i,5,42)
       JumpI3Negative       -> (a,i,0,43)
       JumpI3Zero           -> (a,i,1,43)
       JumpI3Positive       -> (a,i,2,43)
       JumpI3NonNegative    -> (a,i,3,43)
       JumpI3NonZero        -> (a,i,4,43)
       JumpI3NonPositive    -> (a,i,5,43)
       JumpI4Negative       -> (a,i,0,44)
       JumpI4Zero           -> (a,i,1,44)
       JumpI4Positive       -> (a,i,2,44)
       JumpI4NonNegative    -> (a,i,3,44)
       JumpI4NonZero        -> (a,i,4,44)
       JumpI4NonPositive    -> (a,i,5,44)
       JumpI5Negative       -> (a,i,0,45)
       JumpI5Zero           -> (a,i,1,45)
       JumpI5Positive       -> (a,i,2,45)
       JumpI5NonNegative    -> (a,i,3,45)
       JumpI5NonZero        -> (a,i,4,45)
       JumpI5NonPositive    -> (a,i,5,45)
       JumpI6Negative       -> (a,i,0,46)
       JumpI6Zero           -> (a,i,1,46)
       JumpI6Positive       -> (a,i,2,46)
       JumpI6NonNegative    -> (a,i,3,46)
       JumpI6NonZero        -> (a,i,4,46)
       JumpI6NonPositive    -> (a,i,5,46)
       ShiftA               -> (a,i,0,6)
       ShiftX               -> (a,i,1,6)
       ShiftACircular       -> (a,i,2,6)
       ShiftXCircular       -> (a,i,3,6)
       SwapAX               -> (0,0,4,6)
       MoveXI1              -> (0,0,0,7)
       MoveXI2              -> (0,0,1,7)
       MoveXI3              -> (0,0,2,7)
       MoveXI4              -> (0,0,3,7)
       MoveXI5              -> (0,0,4,7)
       MoveXI6              -> (0,0,5,7)
       NoOperation          -> (0,0,0,0)
       Halt                 -> (0,0,2,5)


cleanStatic : StaticInstruction -> StaticInstructionClean
cleanStatic (a,i,m,t) =
    case t of
        LoadA                -> (Just a,Just i,Just m ,t)
        LoadX                -> (Just a,Just i,Just m ,t)
        LoadI1               -> (Just a,Just i,Just m ,t)
        LoadI2               -> (Just a,Just i,Just m ,t)
        LoadI3               -> (Just a,Just i,Just m ,t)
        LoadI4               -> (Just a,Just i,Just m ,t)
        LoadI5               -> (Just a,Just i,Just m ,t)
        LoadI6               -> (Just a,Just i,Just m ,t)
        LoadANeg             -> (Just a,Just i,Just m ,t)
        LoadXNeg             -> (Just a,Just i,Just m ,t)
        LoadI1Neg            -> (Just a,Just i,Just m ,t)
        LoadI2Neg            -> (Just a,Just i,Just m ,t)
        LoadI3Neg            -> (Just a,Just i,Just m ,t)
        LoadI4Neg            -> (Just a,Just i,Just m ,t)
        LoadI5Neg            -> (Just a,Just i,Just m ,t)
        LoadI6Neg            -> (Just a,Just i,Just m ,t)
        StoreA               -> (Just a,Just i,Just m ,t)
        StoreX               -> (Just a,Just i,Just m ,t)
        StoreI1              -> (Just a,Just i,Just m ,t)
        StoreI2              -> (Just a,Just i,Just m ,t)
        StoreI3              -> (Just a,Just i,Just m ,t)
        StoreI4              -> (Just a,Just i,Just m ,t)
        StoreI5              -> (Just a,Just i,Just m ,t)
        StoreI6              -> (Just a,Just i,Just m ,t)
        StoreJ               -> (Just a,Just i,Just m ,t)
        StoreZero            -> (Just a,Just i,Just m ,t)
        Add                  -> (Just a,Just i,Just m ,t)
        Sub                  -> (Just a,Just i,Just m ,t)
        AddX                 -> (Just a,Just i,Just m ,t)
        SubX                 -> (Just a,Just i,Just m ,t)
        EnterA               -> (Just a,Just i,Nothing,t)
        EnterX               -> (Just a,Just i,Nothing,t)
        EnterI1              -> (Just a,Just i,Nothing,t)
        EnterI2              -> (Just a,Just i,Nothing,t)
        EnterI3              -> (Just a,Just i,Nothing,t)
        EnterI4              -> (Just a,Just i,Nothing,t)
        EnterI5              -> (Just a,Just i,Nothing,t)
        EnterI6              -> (Just a,Just i,Nothing,t)
        EnterANeg            -> (Just a,Just i,Nothing,t)
        EnterXNeg            -> (Just a,Just i,Nothing,t)
        EnterI1Neg           -> (Just a,Just i,Nothing,t)
        EnterI2Neg           -> (Just a,Just i,Nothing,t)
        EnterI3Neg           -> (Just a,Just i,Nothing,t)
        EnterI4Neg           -> (Just a,Just i,Nothing,t)
        EnterI5Neg           -> (Just a,Just i,Nothing,t)
        EnterI6Neg           -> (Just a,Just i,Nothing,t)
        IncrementA           -> (Just a,Just i,Nothing,t)
        IncrementX           -> (Just a,Just i,Nothing,t)
        IncrementI1          -> (Just a,Just i,Nothing,t)
        IncrementI2          -> (Just a,Just i,Nothing,t)
        IncrementI3          -> (Just a,Just i,Nothing,t)
        IncrementI4          -> (Just a,Just i,Nothing,t)
        IncrementI5          -> (Just a,Just i,Nothing,t)
        IncrementI6          -> (Just a,Just i,Nothing,t)
        DecrementA           -> (Just a,Just i,Nothing,t)
        DecrementX           -> (Just a,Just i,Nothing,t)
        DecrementI1          -> (Just a,Just i,Nothing,t)
        DecrementI2          -> (Just a,Just i,Nothing,t)
        DecrementI3          -> (Just a,Just i,Nothing,t)
        DecrementI4          -> (Just a,Just i,Nothing,t)
        DecrementI5          -> (Just a,Just i,Nothing,t)
        DecrementI6          -> (Just a,Just i,Nothing,t)
        CompareA             -> (Just a,Just i,Just m ,t)
        CompareX             -> (Just a,Just i,Just m ,t)
        CompareI1            -> (Just a,Just i,Just m ,t)
        CompareI2            -> (Just a,Just i,Just m ,t)
        CompareI3            -> (Just a,Just i,Just m ,t)
        CompareI4            -> (Just a,Just i,Just m ,t)
        CompareI5            -> (Just a,Just i,Just m ,t)
        CompareI6            -> (Just a,Just i,Just m ,t)
        Jump                 -> (Just a,Just i,Nothing,t)
        JumpSaveJ            -> (Just a,Just i,Nothing,t)
        JumpOnOverflow       -> (Just a,Just i,Nothing,t)
        JumpOnNoOverflow     -> (Just a,Just i,Nothing,t)
        JumpOnLess           -> (Just a,Just i,Nothing,t)
        JumpOnEqual          -> (Just a,Just i,Nothing,t)
        JumpOnGreater        -> (Just a,Just i,Nothing,t)
        JumpOnGreaterEqual   -> (Just a,Just i,Nothing,t)
        JumpOnUnEqual        -> (Just a,Just i,Nothing,t)
        JumpOnLessEqual      -> (Just a,Just i,Nothing,t)
        JumpANegative        -> (Just a,Just i,Nothing,t)
        JumpAZero            -> (Just a,Just i,Nothing,t)
        JumpAPositive        -> (Just a,Just i,Nothing,t)
        JumpANonNegative     -> (Just a,Just i,Nothing,t)
        JumpANonZero         -> (Just a,Just i,Nothing,t)
        JumpANonPositive     -> (Just a,Just i,Nothing,t)
        JumpXNegative        -> (Just a,Just i,Nothing,t)
        JumpXZero            -> (Just a,Just i,Nothing,t)
        JumpXPositive        -> (Just a,Just i,Nothing,t)
        JumpXNonNegative     -> (Just a,Just i,Nothing,t)
        JumpXNonZero         -> (Just a,Just i,Nothing,t)
        JumpXNonPositive     -> (Just a,Just i,Nothing,t)
        JumpI1Negative       -> (Just a,Just i,Nothing,t)
        JumpI1Zero           -> (Just a,Just i,Nothing,t)
        JumpI1Positive       -> (Just a,Just i,Nothing,t)
        JumpI1NonNegative    -> (Just a,Just i,Nothing,t)
        JumpI1NonZero        -> (Just a,Just i,Nothing,t)
        JumpI1NonPositive    -> (Just a,Just i,Nothing,t)
        JumpI2Negative       -> (Just a,Just i,Nothing,t)
        JumpI2Zero           -> (Just a,Just i,Nothing,t)
        JumpI2Positive       -> (Just a,Just i,Nothing,t)
        JumpI2NonNegative    -> (Just a,Just i,Nothing,t)
        JumpI2NonZero        -> (Just a,Just i,Nothing,t)
        JumpI2NonPositive    -> (Just a,Just i,Nothing,t)
        JumpI3Negative       -> (Just a,Just i,Nothing,t)
        JumpI3Zero           -> (Just a,Just i,Nothing,t)
        JumpI3Positive       -> (Just a,Just i,Nothing,t)
        JumpI3NonNegative    -> (Just a,Just i,Nothing,t)
        JumpI3NonZero        -> (Just a,Just i,Nothing,t)
        JumpI3NonPositive    -> (Just a,Just i,Nothing,t)
        JumpI4Negative       -> (Just a,Just i,Nothing,t)
        JumpI4Zero           -> (Just a,Just i,Nothing,t)
        JumpI4Positive       -> (Just a,Just i,Nothing,t)
        JumpI4NonNegative    -> (Just a,Just i,Nothing,t)
        JumpI4NonZero        -> (Just a,Just i,Nothing,t)
        JumpI4NonPositive    -> (Just a,Just i,Nothing,t)
        JumpI5Negative       -> (Just a,Just i,Nothing,t)
        JumpI5Zero           -> (Just a,Just i,Nothing,t)
        JumpI5Positive       -> (Just a,Just i,Nothing,t)
        JumpI5NonNegative    -> (Just a,Just i,Nothing,t)
        JumpI5NonZero        -> (Just a,Just i,Nothing,t)
        JumpI5NonPositive    -> (Just a,Just i,Nothing,t)
        JumpI6Negative       -> (Just a,Just i,Nothing,t)
        JumpI6Zero           -> (Just a,Just i,Nothing,t)
        JumpI6Positive       -> (Just a,Just i,Nothing,t)
        JumpI6NonNegative    -> (Just a,Just i,Nothing,t)
        JumpI6NonZero        -> (Just a,Just i,Nothing,t)
        JumpI6NonPositive    -> (Just a,Just i,Nothing,t)
        ShiftA               -> (Just a,Just i,Nothing,t)
        ShiftX               -> (Just a,Just i,Nothing,t)
        ShiftACircular       -> (Just a,Just i,Nothing,t)
        ShiftXCircular       -> (Just a,Just i,Nothing,t)
        SwapAX               -> (Nothing,Nothing,Nothing,t)
        MoveXI1              -> (Nothing,Nothing,Nothing,t)
        MoveXI2              -> (Nothing,Nothing,Nothing,t)
        MoveXI3              -> (Nothing,Nothing,Nothing,t)
        MoveXI4              -> (Nothing,Nothing,Nothing,t)
        MoveXI5              -> (Nothing,Nothing,Nothing,t)
        MoveXI6              -> (Nothing,Nothing,Nothing,t)
        NoOperation          -> (Nothing,Nothing,Nothing,t)
        Halt                 -> (Nothing,Nothing,Nothing,t)
