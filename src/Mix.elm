module Mix exposing ( Address
                    , Index
                    , Modification
                    , InstructionCode
                    , Memory
                    , read
                    , Mix
                    , RuntimeError(..)
                    , MixOperation
                    , Instruction(..)
                    , step
                    , instruction
                    )

import Dict

import Atom exposing ( Base
                     , Sign(..)
                     , swap
                     , baseExpand
                     , baseExpandPad
                     , baseContract
                     , mixBase
                     , Byte
                     , byte
                     , zero
                     , zeroWord
                     , zeroSmallWord
                     , value
                     , SmallWord
                     , Word
                     , wordExpand
                     , wordContract
                     , Mask(..)
                     , Masks
                     , maskFilter
                     , copy
                     , wordValue
                     , smallWordValue
                     , byteToMasks
                     , masksToByte
                     , flipSignWord
                     , flipSignSmallWord
                     , OverflowToggle(..)
                     , ComparisonIndicator(..)
                     , op
                     , intToWord
                     , intToSmallWord
                     , comp
                     , shift
                     , shiftCircular
                     )
{-

execution cycle:
  unpack instruction
  increment program counter
  decode instruction
  execute instruction

-}


type alias Address = Int
type alias Index = Int
type alias Modification = Int
type alias InstructionCode = Int
type alias Memory = Dict.Dict Address Word

-- the default memory value is zeroWord

read : Address -> Memory -> Word
read a mem = Maybe.withDefault zeroWord <| Dict.get a mem
    
type alias Mix = { a   : Word
                 , x   : Word
                 , i1  : SmallWord
                 , i2  : SmallWord
                 , i3  : SmallWord
                 , i4  : SmallWord
                 , i5  : SmallWord
                 , i6  : SmallWord
                 , j   : SmallWord
                 , p   : Address -- program counter
                 , mem : Memory
                 , overflow : OverflowToggle
                 , comparison : ComparisonIndicator
                 }

type RuntimeError = NoMemoryValue Address
                  | InvalidModification Modification
                  | UnrecognizedInstructionCode InstructionCode
                  | InvalidIndex Index


type alias MixOperation a = Mix -> Result RuntimeError (Mix,a)



-- unpack instruction
type alias UnpackedWord = (Address,Modification,Masks,InstructionCode)
unpackInstructionAddress : Mix -> Address -> MixOperation UnpackedWord
unpackInstructionAddress m a =
    case Dict.get a m.mem of
        Nothing                 -> throwError <| NoMemoryValue m.p
        Just (s,b1,b2,b3,b4,b5) -> let f v = return
                                             ( (smallWordValue (s,b1,b2) + v)
                                             , value b4
                                             , byteToMasks b4
                                             , value b5
                                             )
                                   in case value b3 of
                                          0 -> f 0 
                                          1 -> f <| smallWordValue <| m.i1
                                          2 -> f <| smallWordValue <| m.i2
                                          3 -> f <| smallWordValue <| m.i3
                                          4 -> f <| smallWordValue <| m.i4
                                          5 -> f <| smallWordValue <| m.i5
                                          6 -> f <| smallWordValue <| m.i6
                                          _ -> throwError <| InvalidIndex
                                                          <| value b3


unpackInstruction : Mix -> MixOperation UnpackedWord
unpackInstruction m = unpackInstructionAddress m m.p

instruction : Mix -> Address -> Result Word Instruction
instruction m a =
    let p = (unpackInstructionAddress m a) >>= decodeInstruction
    in case p m of
           Err err  -> Err <| read a m.mem
           Ok (s,i) -> Ok i

-- increment program counter
incrementCounter : MixOperation () 
incrementCounter =
    let op m = { m | p = m.p + 1 }
    in (op <$> get) >>= put 

{-

design point:

we are not implementing multiplication and division at a machine level.
Instead, we have AddX and SubX.
Multiplication / Division are easily implemented as routines.

-}


type Instruction = LoadA Address Masks         -- LDA i,adr,mask
                 | LoadX Address Masks         -- LDX i,adr,mask
                 | LoadI1 Address Masks        -- LD1 i,adr,mask
                 | LoadI2 Address Masks        -- LD2 i,adr,mask
                 | LoadI3 Address Masks        -- LD3 i,adr,mask
                 | LoadI4 Address Masks        -- LD4 i,adr,mask
                 | LoadI5 Address Masks        -- LD5 i,adr,mask
                 | LoadI6 Address Masks        -- LD6 i,adr,mask
                 | LoadANeg Address Masks      -- LDAN i,adr,mask
                 | LoadXNeg Address Masks      -- LDXN i,adr,mask
                 | LoadI1Neg Address Masks     -- LD1N i,adr,mask
                 | LoadI2Neg Address Masks     -- LD2N i,adr,mask
                 | LoadI3Neg Address Masks     -- LD3N i,adr,mask
                 | LoadI4Neg Address Masks     -- LD4N i,adr,mask
                 | LoadI5Neg Address Masks     -- LD5N i,adr,mask
                 | LoadI6Neg Address Masks     -- LD6N i,adr,mask
                 | StoreA Address Masks        -- STA i,adr,mask
                 | StoreX Address Masks        -- STX i,adr,mask
                 | StoreI1 Address Masks       -- ST1 i,adr,mask
                 | StoreI2 Address Masks       -- ST2 i,adr,mask     
                 | StoreI3 Address Masks       -- ST3 i,adr,mask
                 | StoreI4 Address Masks       -- ST4 i,adr,mask
                 | StoreI5 Address Masks       -- ST5 i,adr,mask
                 | StoreI6 Address Masks       -- ST6 i,adr,mask
                 | StoreJ Address Masks        -- STJ i,adr,mask
                 | StoreZero Address Masks     -- STZ i,adr,mask
                 | Add Address Masks           -- ADD i,adr,mask
                 | Sub Address Masks           -- SUB i,adr,mask
                 | AddX Masks                  -- ADDX i,adr,mask
                 | SubX Masks                  -- SUBX i,adr,mask
                 | EnterA Address              -- ENTA i,adr
                 | EnterX Address              -- ENTX i,adr
                 | EnterI1 Address             -- ENT1 i,adr
                 | EnterI2 Address             -- ENT2 i,adr
                 | EnterI3 Address             -- ENT3 i,adr
                 | EnterI4 Address             -- ENT4 i,adr
                 | EnterI5 Address             -- ENT5 i,adr
                 | EnterI6 Address             -- ENT6 i,adr
                 | EnterANeg Address           -- ENNA i,adr
                 | EnterXNeg Address           -- ENNX i,adr
                 | EnterI1Neg Address          -- ENN1 i,adr
                 | EnterI2Neg Address          -- ENN2 i,adr
                 | EnterI3Neg Address          -- ENN3 i,adr
                 | EnterI4Neg Address          -- ENN4 i,adr
                 | EnterI5Neg Address          -- ENN5 i,adr
                 | EnterI6Neg Address          -- ENN6 i,adr
                 | IncrementA Address          -- INCA i,adr
                 | IncrementX Address          -- INCX i,adr
                 | IncrementI1 Address         -- INC1 i,adr
                 | IncrementI2 Address         -- INC2 i,adr
                 | IncrementI3 Address         -- INC3 i,adr
                 | IncrementI4 Address         -- INC4 i,adr
                 | IncrementI5 Address         -- INC5 i,adr
                 | IncrementI6 Address         -- INC6 i,adr
                 | DecrementA Address          -- DECA i,adr
                 | DecrementX Address          -- DECX i,adr
                 | DecrementI1 Address         -- DEC1 i,adr
                 | DecrementI2 Address         -- DEC2 i,adr
                 | DecrementI3 Address         -- DEC3 i,adr
                 | DecrementI4 Address         -- DEC4 i,adr
                 | DecrementI5 Address         -- DEC5 i,adr
                 | DecrementI6 Address         -- DEC6 i,adr
                 | CompareA Address Masks      -- CMPA i,adr,mask
                 | CompareX Address Masks      -- CMPX i,adr,mask
                 | CompareI1 Address Masks     -- CMP1 i,adr,mask
                 | CompareI2 Address Masks     -- CMP2 i,adr,mask
                 | CompareI3 Address Masks     -- CMP3 i,adr,mask
                 | CompareI4 Address Masks     -- CMP4 i,adr,mask
                 | CompareI5 Address Masks     -- CMP5 i,adr,mask
                 | CompareI6 Address Masks     -- CMP6 i,adr,mask
                 | Jump Address                -- JMP i,adr
                 | JumpSaveJ Address           -- JSJ i,adr
                 | JumpOnOverflow Address      -- JOV i,adr
                 | JumpOnNoOverflow Address    -- JNOV i,adr
                 | JumpOnLess Address          -- JL i,adr
                 | JumpOnEqual Address         -- JE i,adr
                 | JumpOnGreater Address       -- JG i,adr
                 | JumpOnGreaterEqual Address  -- JGE i,adr
                 | JumpOnUnEqual Address       -- JNE i,adr
                 | JumpOnLessEqual Address     -- JLE i,adr
                 | JumpANegative Address       -- JAN i,adr
                 | JumpAZero Address           -- JAZ i,adr
                 | JumpAPositive Address       -- JAP i,adr
                 | JumpANonNegative Address    -- JANN i,adr
                 | JumpANonZero Address        -- JANZ i,adr
                 | JumpANonPositive Address    -- JANP i,adr
                 | JumpXNegative Address       -- JXN i,adr
                 | JumpXZero Address           -- JXZ i,adr
                 | JumpXPositive Address       -- JXP i,adr
                 | JumpXNonNegative Address    -- JXNN i,adr
                 | JumpXNonZero Address        -- JXNZ i,adr
                 | JumpXNonPositive Address    -- JXNP i,adr
                 | JumpI1Negative Address      -- J1N i,adr
                 | JumpI1Zero Address          -- J1Z i,adr
                 | JumpI1Positive Address      -- J1P i,adr
                 | JumpI1NonNegative Address   -- J1NN i,adr
                 | JumpI1NonZero Address       -- J1NZ i,adr
                 | JumpI1NonPositive Address   -- J1NP i,adr
                 | JumpI2Negative Address      -- J2N i,adr 
                 | JumpI2Zero Address          -- J2Z i,adr
                 | JumpI2Positive Address      -- J2P i,adr
                 | JumpI2NonNegative Address   -- J2NN i,adr
                 | JumpI2NonZero Address       -- J2NZ i,adr
                 | JumpI2NonPositive Address   -- J2NP i,adr
                 | JumpI3Negative Address      -- J3N i,adr
                 | JumpI3Zero Address          -- J3Z i,adr
                 | JumpI3Positive Address      -- J3P i,adr
                 | JumpI3NonNegative Address   -- J3NN i,adr
                 | JumpI3NonZero Address       -- J3NZ i,adr
                 | JumpI3NonPositive Address   -- J3NP i,adr
                 | JumpI4Negative Address      -- J4N i,adr
                 | JumpI4Zero Address          -- J4Z i,adr
                 | JumpI4Positive Address      -- J4P i,adr
                 | JumpI4NonNegative Address   -- J4NN i,adr
                 | JumpI4NonZero Address       -- J4NZ i,adr
                 | JumpI4NonPositive Address   -- J4NP i,adr
                 | JumpI5Negative Address      -- J5N i,adr
                 | JumpI5Zero Address          -- J5Z i,adr
                 | JumpI5Positive Address      -- J5P i,adr
                 | JumpI5NonNegative Address   -- J5NN i,adr
                 | JumpI5NonZero Address       -- J5NZ i,adr
                 | JumpI5NonPositive Address   -- J5NP i,adr
                 | JumpI6Negative Address      -- J6N i,adr
                 | JumpI6Zero Address          -- J6Z i,adr
                 | JumpI6Positive Address      -- J6P i,adr
                 | JumpI6NonNegative Address   -- J6NN i,adr
                 | JumpI6NonZero Address       -- J6NZ i,adr
                 | JumpI6NonPositive Address   -- J6NP i,adr
                 | ShiftA Address              -- SA i,adr
                 | ShiftX Address              -- SX i,adr
                 | ShiftACircular Address      -- SAC i,adr
                 | ShiftXCircular Address      -- SAX i,adr
                 | SwapAX                      -- SWAP
                 | MoveXI1                     -- MOVX1
                 | MoveXI2                     -- MOVX2
                 | MoveXI3                     -- MOVX3
                 | MoveXI4                     -- MOVX4
                 | MoveXI5                     -- MOVX5
                 | MoveXI6                     -- MOVX6
                 | NoOperation                 -- NOP
                 | Halt                        -- HLT

{-

when adding a new instruction, you need to update
  decodeInstruction
  executeInstructionTransition

-}

decodeInstruction : UnpackedWord -> MixOperation Instruction
decodeInstruction (a,f,ms,c) =
    case c of
        0  -> return NoOperation
        8  -> return <| LoadA a ms
        15 -> return <| LoadX a ms
        9  -> return <| LoadI1 a ms
        10 -> return <| LoadI2 a ms
        11 -> return <| LoadI3 a ms
        12 -> return <| LoadI4 a ms
        13 -> return <| LoadI5 a ms
        14 -> return <| LoadI6 a ms
        16 -> return <| LoadANeg a ms
        23 -> return <| LoadXNeg a ms
        17 -> return <| LoadI1Neg a ms
        18 -> return <| LoadI2Neg a ms
        19 -> return <| LoadI3Neg a ms
        20 -> return <| LoadI4Neg a ms
        21 -> return <| LoadI5Neg a ms
        22 -> return <| LoadI6Neg a ms
        24 -> return <| StoreA a ms
        31 -> return <| StoreX a ms
        25 -> return <| StoreI1 a ms
        26 -> return <| StoreI2 a ms
        27 -> return <| StoreI3 a ms
        28 -> return <| StoreI4 a ms
        29 -> return <| StoreI5 a ms
        30 -> return <| StoreI6 a ms
        32 -> return <| StoreJ a ms
        33 -> return <| StoreZero a ms
        1  -> return <| Add a ms
        2  -> return <| Sub a ms
        3  -> return <| AddX ms
        4  -> return <| SubX ms
        56 -> return <| CompareA a ms
        63 -> return <| CompareX a ms
        57 -> return <| CompareI1 a ms
        58 -> return <| CompareI2 a ms
        59 -> return <| CompareI3 a ms
        60 -> return <| CompareI4 a ms
        61 -> return <| CompareI5 a ms
        62 -> return <| CompareI6 a ms
        48 -> case f of
                  2 -> return <| EnterA a
                  3 -> return <| EnterANeg a
                  0 -> return <| IncrementA a
                  1 -> return <| DecrementA a
                  y -> throwError <| InvalidModification f
        55 -> case f of
                  2 -> return <| EnterX a
                  3 -> return <| EnterXNeg a
                  0 -> return <| IncrementX a
                  1 -> return <| DecrementX a
                  y -> throwError <| InvalidModification f
        49 -> case f of
                  2 -> return <| EnterI1 a
                  3 -> return <| EnterI1Neg a
                  0 -> return <| IncrementI1 a
                  1 -> return <| DecrementI1 a
                  y -> throwError <| InvalidModification f
        50 -> case f of
                  2 -> return <| EnterI2 a
                  3 -> return <| EnterI2Neg a
                  0 -> return <| IncrementI2 a
                  1 -> return <| DecrementI2 a
                  y -> throwError <| InvalidModification f
        51 -> case f of
                  2 -> return <| EnterI3 a
                  3 -> return <| EnterI3Neg a
                  0 -> return <| IncrementI3 a
                  1 -> return <| DecrementI3 a
                  y -> throwError <| InvalidModification f
        52 -> case f of
                  2 -> return <| EnterI4 a
                  3 -> return <| EnterI4Neg a
                  0 -> return <| IncrementI4 a
                  1 -> return <| DecrementI4 a
                  y -> throwError <| InvalidModification f
        53 -> case f of
                  2 -> return <| EnterI5 a
                  3 -> return <| EnterI5Neg a
                  0 -> return <| IncrementI5 a
                  1 -> return <| DecrementI5 a
                  y -> throwError <| InvalidModification f
        54 -> case f of
                  2 -> return <| EnterI6 a
                  3 -> return <| EnterI6Neg a
                  0 -> return <| IncrementI6 a
                  1 -> return <| DecrementI6 a
                  y -> throwError <| InvalidModification f
        39 -> case f of
                  0 -> return <| Jump a
                  1 -> return <| JumpSaveJ a
                  2 -> return <| JumpOnOverflow a
                  3 -> return <| JumpOnNoOverflow a
                  4 -> return <| JumpOnLess a
                  5 -> return <| JumpOnEqual a
                  6 -> return <| JumpOnGreater a
                  7 -> return <| JumpOnGreaterEqual a
                  8 -> return <| JumpOnUnEqual a
                  9 -> return <| JumpOnLessEqual a
                  y -> throwError <| InvalidModification f
        40 -> case f of
                  0 -> return <| JumpANegative a
                  1 -> return <| JumpAZero a
                  2 -> return <| JumpAPositive a
                  3 -> return <| JumpANonNegative a
                  4 -> return <| JumpANonZero a
                  5 -> return <| JumpANonPositive a
                  y -> throwError <| InvalidModification f
        47 -> case f of
                  0 -> return <| JumpXNegative a
                  1 -> return <| JumpXZero a
                  2 -> return <| JumpXPositive a
                  3 -> return <| JumpXNonNegative a
                  4 -> return <| JumpXNonZero a
                  5 -> return <| JumpXNonPositive a
                  y -> throwError <| InvalidModification f
        41 -> case f of
                  0 -> return <| JumpI1Negative a
                  1 -> return <| JumpI1Zero a
                  2 -> return <| JumpI1Positive a
                  3 -> return <| JumpI1NonNegative a
                  4 -> return <| JumpI1NonZero a
                  5 -> return <| JumpI1NonPositive a
                  y -> throwError <| InvalidModification f
        42 -> case f of
                  0 -> return <| JumpI2Negative a
                  1 -> return <| JumpI2Zero a
                  2 -> return <| JumpI2Positive a
                  3 -> return <| JumpI2NonNegative a
                  4 -> return <| JumpI2NonZero a
                  5 -> return <| JumpI2NonPositive a
                  y -> throwError <| InvalidModification f
        43 -> case f of
                  0 -> return <| JumpI3Negative a
                  1 -> return <| JumpI3Zero a
                  2 -> return <| JumpI3Positive a
                  3 -> return <| JumpI3NonNegative a
                  4 -> return <| JumpI3NonZero a
                  5 -> return <| JumpI3NonPositive a
                  y -> throwError <| InvalidModification f
        44 -> case f of
                  0 -> return <| JumpI4Negative a
                  1 -> return <| JumpI4Zero a
                  2 -> return <| JumpI4Positive a
                  3 -> return <| JumpI4NonNegative a
                  4 -> return <| JumpI4NonZero a
                  5 -> return <| JumpI4NonPositive a
                  y -> throwError <| InvalidModification f
        45 -> case f of
                  0 -> return <| JumpI5Negative a
                  1 -> return <| JumpI5Zero a
                  2 -> return <| JumpI5Positive a
                  3 -> return <| JumpI5NonNegative a
                  4 -> return <| JumpI5NonZero a
                  5 -> return <| JumpI5NonPositive a
                  y -> throwError <| InvalidModification f
        46 -> case f of
                  0 -> return <| JumpI6Negative a
                  1 -> return <| JumpI6Zero a
                  2 -> return <| JumpI6Positive a
                  3 -> return <| JumpI6NonNegative a
                  4 -> return <| JumpI6NonZero a
                  5 -> return <| JumpI6NonPositive a
                  y -> throwError <| InvalidModification f
        6  -> case f of
                  0 -> return <| ShiftA a
                  1 -> return <| ShiftX a
                  2 -> return <| ShiftACircular a
                  3 -> return <| ShiftXCircular a
                  4 -> return SwapAX
                  y -> throwError <| InvalidModification f
        7  -> case f of
                  0 -> return MoveXI1
                  1 -> return MoveXI2
                  2 -> return MoveXI3
                  3 -> return MoveXI4
                  4 -> return MoveXI5
                  5 -> return MoveXI6
                  y -> throwError <| InvalidModification f
        5  -> case f of
                  2 -> return Halt
                  y -> throwError <| InvalidModification f
        x  -> throwError <| UnrecognizedInstructionCode x



executeInstructionTransition : Instruction -> Mix -> Mix
executeInstructionTransition i s =
    case i of
        LoadA adr masks
            -> { s | a = copy masks (read adr s.mem) s.a }
        LoadX adr masks
            -> { s | x = copy masks (read adr s.mem) s.x }
        LoadI1 adr masks
            -> { s | i1 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i1 }
        LoadI2 adr masks
            -> { s | i2 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i2 }
        LoadI3 adr masks
            -> { s | i3 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i3 }
        LoadI4 adr masks
            -> { s | i4 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i4 }
        LoadI5 adr masks
            -> { s | i5 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i5 }
        LoadI6 adr masks
            -> { s | i6 = wordContract
                        <| copy masks (read adr s.mem)
                        <| wordExpand s.i6 }
        LoadANeg adr masks
            -> { s | a = copy masks (flipSignWord <| read adr s.mem) s.a }
        LoadXNeg adr masks
            -> { s | x = copy masks (flipSignWord <| read adr s.mem) s.x }
        LoadI1Neg adr masks
            -> { s | i1 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i1 }
        LoadI2Neg adr masks
            -> { s | i2 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i2 }
        LoadI3Neg adr masks
            -> { s | i3 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i3 }
        LoadI4Neg adr masks
            -> { s | i4 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i4 }
        LoadI5Neg adr masks
            -> { s | i5 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i5 }
        LoadI6Neg adr masks
            -> { s | i6 = wordContract
                        <| copy masks (flipSignWord <| read adr s.mem)
                        <| wordExpand s.i6 }
        StoreA adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks s.a <| read adr s.mem)
                     s.mem
               }
        StoreX adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks s.x <| read adr s.mem)
                     s.mem
               }
        StoreI1 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i1) <| read adr s.mem)
                     s.mem
               }
        StoreI2 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i2) <| read adr s.mem)
                     s.mem
               }
        StoreI3 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i3) <| read adr s.mem)
                     s.mem
               }
        StoreI4 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i4) <| read adr s.mem)
                     s.mem
               }
        StoreI5 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i5) <| read adr s.mem)
                     s.mem
               }
        StoreI6 adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.i6) <| read adr s.mem)
                     s.mem
               }
        StoreJ adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks (wordExpand s.j) <| read adr s.mem)
                     s.mem
               }
        StoreZero adr masks
            -> { s | mem = Dict.insert adr
                     (copy masks zeroWord <| read adr s.mem)
                     s.mem
               }
        Add adr masks
            -> let (t,r) = op (+) masks s.a <| read adr s.mem
               in { s | a = r
                      , overflow = t
                  }
        Sub adr masks
            -> let (t,r) = op (-) masks s.a <| read adr s.mem
               in { s | a = r
                      , overflow = t
                  }
        AddX masks
            -> let (t,r) = op (+) masks s.a <| s.x
               in { s | a = r
                      , overflow = t
                  }
        SubX masks
            -> let (t,r) = op (-) masks s.a <| s.x
               in { s | a = r
                  , overflow = t
                  }
        EnterA adr
            -> let (t,r) = intToWord adr s.a
               in { s | a = r
                  , overflow = t
                  }
        EnterX adr
            -> let (t,r) = intToWord adr s.x
               in { s | x = r
                  , overflow = t
                  }
        EnterI1 adr
            -> let (t,r) = intToSmallWord adr s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        EnterI2 adr
            -> let (t,r) = intToSmallWord adr s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        EnterI3 adr
            -> let (t,r) = intToSmallWord adr s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        EnterI4 adr
            -> let (t,r) = intToSmallWord adr s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        EnterI5 adr
            -> let (t,r) = intToSmallWord adr s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        EnterI6 adr
            -> let (t,r) = intToSmallWord adr s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        EnterANeg adr
            -> let (t,r) = intToWord (negate adr) s.a
               in { s | a = r
                  , overflow = t
                  }
        EnterXNeg adr
            -> let (t,r) = intToWord (negate adr) s.x
               in { s | x = r
                  , overflow = t
                  }
        EnterI1Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        EnterI2Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        EnterI3Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        EnterI4Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        EnterI5Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        EnterI6Neg adr
            -> let (t,r) = intToSmallWord (negate adr) s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        IncrementA adr
            -> let (t,r) = intToWord ( (wordValue s.a) + adr) s.a
               in { s | a = r
                  , overflow = t
                  }
        IncrementX adr
            -> let (t,r) = intToWord ( (wordValue s.x) + adr) s.x
               in { s | x = r
                  , overflow = t
                  }
        IncrementI1 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i1) + adr) s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        IncrementI2 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i2) + adr) s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        IncrementI3 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i3) + adr) s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        IncrementI4 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i4) + adr) s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        IncrementI5 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i5) + adr) s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        IncrementI6 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i6) + adr) s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        DecrementA adr
            -> let (t,r) = intToWord ( (wordValue s.a) - adr) s.a
               in { s | a = r
                  , overflow = t
                  }
        DecrementX adr
            -> let (t,r) = intToWord ( (wordValue s.x) - adr) s.x
               in { s | x = r
                  , overflow = t
                  }
        DecrementI1 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i1) - adr) s.i1
               in { s | i1 = r
                  , overflow = t
                  }
        DecrementI2 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i2) - adr) s.i2
               in { s | i2 = r
                  , overflow = t
                  }
        DecrementI3 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i3) - adr) s.i3
               in { s | i3 = r
                  , overflow = t
                  }
        DecrementI4 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i4) - adr) s.i4
               in { s | i4 = r
                  , overflow = t
                  }
        DecrementI5 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i5) - adr) s.i5
               in { s | i5 = r
                  , overflow = t
                  }
        DecrementI6 adr
            -> let (t,r) = intToSmallWord ( (smallWordValue s.i6) - adr) s.i6
               in { s | i6 = r
                  , overflow = t
                  }
        CompareA adr masks
            -> let c = comp masks s.a <| read adr s.mem
               in { s | comparison = c }
        CompareX adr masks
            -> let c = comp masks s.x <| read adr s.mem
               in { s | comparison = c }
        CompareI1 adr masks
            -> let c = comp masks (wordExpand s.i1) <| read adr s.mem
               in { s | comparison = c }
        CompareI2 adr masks
            -> let c = comp masks (wordExpand s.i2) <| read adr s.mem
               in { s | comparison = c }
        CompareI3 adr masks
            -> let c = comp masks (wordExpand s.i3) <| read adr s.mem
               in { s | comparison = c }
        CompareI4 adr masks
            -> let c = comp masks (wordExpand s.i4) <| read adr s.mem
               in { s | comparison = c }
        CompareI5 adr masks
            -> let c = comp masks (wordExpand s.i5) <| read adr s.mem
               in { s | comparison = c }
        CompareI6 adr masks
            -> let c = comp masks (wordExpand s.i6) <| read adr s.mem
               in { s | comparison = c }
        Jump adr
            -> let (t,newJ) = intToSmallWord s.p s.j
               in { s
                  | p = adr
                  , j = newJ
                  }
        JumpSaveJ adr
            -> { s | p = adr }
        JumpOnOverflow adr
            -> if s.overflow == Overflow
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       , overflow = Good
                       }
               else s
        JumpOnNoOverflow adr
            -> if s.overflow == Good
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else { s | overflow = Good }
        JumpOnLess adr
            -> if s.comparison == L
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnEqual adr
            -> if s.comparison == E
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnGreater adr
            -> if s.comparison == G
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnGreaterEqual adr
            -> if (s.comparison == G) || (s.comparison == E)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnUnEqual adr
            -> if (s.comparison == L) || (s.comparison == G)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpOnLessEqual adr
            -> if (s.comparison == L) || (s.comparison == E)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANegative adr
            -> if (wordValue s.a < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpAZero adr
            -> if (wordValue s.a == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpAPositive adr
            -> if (wordValue s.a > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANonNegative adr
            -> if (wordValue s.a > 0) || (wordValue s.a == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANonZero adr
            -> if (wordValue s.a < 0) || (wordValue s.a > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpANonPositive adr
            -> if (wordValue s.a < 0) || (wordValue s.a == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNegative adr
            -> if (wordValue s.x < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXZero adr
            -> if (wordValue s.x == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXPositive adr
            -> if (wordValue s.x > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNonNegative adr
            -> if (wordValue s.x > 0) || (wordValue s.x == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNonZero adr
            -> if (wordValue s.x < 0) || (wordValue s.x > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpXNonPositive adr
            -> if (wordValue s.x < 0) || (wordValue s.x == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1Negative adr
            -> if (smallWordValue s.i1 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1Zero adr
            -> if (smallWordValue s.i1 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1Positive adr
            -> if (smallWordValue s.i1 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1NonNegative adr
            -> if (smallWordValue s.i1 > 0) || (smallWordValue s.i1 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1NonZero adr
            -> if (smallWordValue s.i1 < 0) || (smallWordValue s.i1 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI1NonPositive adr
            -> if (smallWordValue s.i1 < 0) || (smallWordValue s.i1 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2Negative adr
            -> if (smallWordValue s.i2 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2Zero adr
            -> if (smallWordValue s.i2 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2Positive adr
            -> if (smallWordValue s.i2 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2NonNegative adr
            -> if (smallWordValue s.i2 > 0) || (smallWordValue s.i2 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2NonZero adr
            -> if (smallWordValue s.i2 < 0) || (smallWordValue s.i2 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI2NonPositive adr
            -> if (smallWordValue s.i2 < 0) || (smallWordValue s.i2 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3Negative adr
            -> if (smallWordValue s.i3 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3Zero adr
            -> if (smallWordValue s.i3 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3Positive adr
            -> if (smallWordValue s.i3 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3NonNegative adr
            -> if (smallWordValue s.i3 > 0) || (smallWordValue s.i3 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3NonZero adr
            -> if (smallWordValue s.i3 < 0) || (smallWordValue s.i3 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI3NonPositive adr
            -> if (smallWordValue s.i3 < 0) || (smallWordValue s.i3 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4Negative adr
            -> if (smallWordValue s.i4 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4Zero adr
            -> if (smallWordValue s.i4 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4Positive adr
            -> if (smallWordValue s.i4 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4NonNegative adr
            -> if (smallWordValue s.i4 > 0) || (smallWordValue s.i4 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4NonZero adr
            -> if (smallWordValue s.i4 < 0) || (smallWordValue s.i4 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI4NonPositive adr
            -> if (smallWordValue s.i4 < 0) || (smallWordValue s.i4 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5Negative adr
            -> if (smallWordValue s.i5 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5Zero adr
            -> if (smallWordValue s.i5 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5Positive adr
            -> if (smallWordValue s.i5 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5NonNegative adr
            -> if (smallWordValue s.i5 > 0) || (smallWordValue s.i5 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5NonZero adr
            -> if (smallWordValue s.i5 < 0) || (smallWordValue s.i5 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI5NonPositive adr
            -> if (smallWordValue s.i5 < 0) || (smallWordValue s.i5 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6Negative adr
            -> if (smallWordValue s.i6 < 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6Zero adr
            -> if (smallWordValue s.i6 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6Positive adr
            -> if (smallWordValue s.i6 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6NonNegative adr
            -> if (smallWordValue s.i6 > 0) || (smallWordValue s.i6 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6NonZero adr
            -> if (smallWordValue s.i6 < 0) || (smallWordValue s.i6 > 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        JumpI6NonPositive adr
            -> if (smallWordValue s.i6 < 0) || (smallWordValue s.i6 == 0)
               then let (t,newJ) = intToSmallWord s.p s.j
                    in { s
                       | p = adr
                       , j = newJ
                       }
               else s
        ShiftA adr
            -> { s | a = shift adr s.a }
        ShiftX adr
            -> { s | x = shift adr s.x }
        ShiftACircular adr
            -> { s | a = shiftCircular adr s.a }
        ShiftXCircular adr
            -> { s | x = shiftCircular adr s.x }
        SwapAX
            -> { s | a = s.x , x = s.a }
        MoveXI1
            -> { s | i1 = wordContract s.x }
        MoveXI2
            -> { s | i2 = wordContract s.x }
        MoveXI3
            -> { s | i3 = wordContract s.x }
        MoveXI4
            -> { s | i4 = wordContract s.x }
        MoveXI5
            -> { s | i5 = wordContract s.x }
        MoveXI6
            -> { s | i6 = wordContract s.x }
        NoOperation
            -> s
        Halt
            -> s



executeInstruction : Instruction -> MixOperation Instruction
executeInstruction i =
    (((executeInstructionTransition i) <$> get) >>= put) *> (return i)


step : MixOperation Instruction
step =
    ((get >>= unpackInstruction) <* incrementCounter)
    >>= decodeInstruction
    >>= executeInstruction




-- monad operations

(>>=) : MixOperation a -> (a -> MixOperation b) -> MixOperation b
(>>=) p f = let q s = case p s of
                          Err err -> Err err
                          Ok (ss,x) -> f x ss
            in q

return : a -> MixOperation a
return x = let p s = Ok (s,x)
           in p

get : MixOperation Mix
get = let p s = Ok (s,s)
      in p

put : Mix -> MixOperation ()
put m = let p s = Ok (m,())
        in p

throwError : RuntimeError -> MixOperation a
throwError err = let p s = Err err
                 in p

(<*>) : MixOperation (a -> b) -> MixOperation a -> MixOperation b
(<*>) p q = let r s = case p s of
                          Err err -> Err err
                          Ok (ss,f) -> case q ss of
                                           Err err -> Err err
                                           Ok (sss,x) -> Ok (sss, f x)
            in r

                
(<$>) : ( a -> b ) -> MixOperation a -> MixOperation b
(<$>) f p = (return f) <*> p


map2 : ( a -> b -> c) -> MixOperation a -> MixOperation b -> MixOperation c
map2 f p q = ( f <$> p ) <*> q


(<*) : MixOperation a -> MixOperation b -> MixOperation a
(<*) p q = let f x y = x
           in map2 f p q

(*>) : MixOperation a -> MixOperation b -> MixOperation b
(*>) p q = let g x y = y
           in map2 g p q


-- test states
testLoad : Mix
testLoad =
    let
        b = masksToByte (On,Off,Off,Off,Off,Off)
        m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 1,b,byte 23))
                          , (1899,(Pos,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = zeroWord
       , x = zeroWord
       , i1 = (Neg,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }


testStore : Mix
testStore =
    let
        b = masksToByte (On,Off,On,Off,Off,Off)
        m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 1,b,byte 25))
                          , (1899,(Pos,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = (Pos,byte 6,byte 7,byte 8,byte 9,byte 0)
       , x = zeroWord
       , i1 = (Neg,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }


testAdd : Mix
testAdd =
    let
        b = masksToByte (On,Off,Off,Off,Off,Off)
        m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 1,b,byte 1))
                          , (1899,(Neg,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = (Pos,byte 1,byte 0,byte 0,byte 0,byte 0)
       , x = zeroWord
       , i1 = (Neg,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }


testEnter : Mix
testEnter =
    let
        m = Dict.fromList [ (0,(Neg,byte 20,byte 0,byte 1,byte 2,byte 54))
                          , (1899,(Neg,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = (Pos,byte 1,byte 0,byte 0,byte 0,byte 0)
       , x = zeroWord
       , i1 = (Neg,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }


testJump : Mix
testJump =
    let
        m = Dict.fromList [ (0,(Pos,byte 0,byte 53,byte 1,byte 0,byte 39))
                          , (1899,(Neg,byte 1,byte 2,byte 3,byte 4,byte 5))
                          ]
    in { a = (Pos,byte 1,byte 0,byte 0,byte 0,byte 0)
       , x = zeroWord
       , i1 = (Pos,byte 1,byte 1)
       , i2 = zeroSmallWord
       , i3 = zeroSmallWord
       , i4 = zeroSmallWord
       , i5 = zeroSmallWord
       , i6 = zeroSmallWord
       , j = zeroSmallWord
       , p = 0
       , mem = m
       , overflow = Good
       , comparison = E
       }
