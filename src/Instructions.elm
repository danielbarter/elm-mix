module Instructions exposing ( Instruction(..)
                             , mapInstruction
                             , distributeResult
                             , unpack
                             , Modification
                             , InstructionCode
                             , Address
                             , decodeInstruction
                             , encodeInstruction
                             , Index
                             , UnpackedWord
                             , StaticInstruction
                             , DynamicInstruction
                             , CompileTimeError(..)
                             )

import Atom exposing (..)

type alias Modification = Int
type alias InstructionCode = Int
type alias Address = Int
type alias Index = Int
type alias UnpackedWord = (Address,Index,Modification,Masks,InstructionCode)


unpack : Word -> UnpackedWord
unpack (s,b1,b2,b3,b4,b5) = ( smallWordValue (s,b1,b2)
                            , value b3
                            , value b4
                            , byteToMasks b4
                            , value b5
                            )

type alias StaticInstruction = Instruction (Index,Address)
type alias DynamicInstruction = Instruction Address


type Instruction a = LoadA a Masks         -- n: LDA,adr,i,m
                   | LoadX a Masks         -- LDX 
                   | LoadI1 a Masks        -- LD1 
                   | LoadI2 a Masks        -- LD2 
                   | LoadI3 a Masks        -- LD3 
                   | LoadI4 a Masks        -- LD4 
                   | LoadI5 a Masks        -- LD5 
                   | LoadI6 a Masks        -- LD6 
                   | LoadANeg a Masks      -- LDAN 
                   | LoadXNeg a Masks      -- LDXN 
                   | LoadI1Neg a Masks     -- LD1N 
                   | LoadI2Neg a Masks     -- LD2N 
                   | LoadI3Neg a Masks     -- LD3N 
                   | LoadI4Neg a Masks     -- LD4N 
                   | LoadI5Neg a Masks     -- LD5N 
                   | LoadI6Neg a Masks     -- LD6N 
                   | StoreA a Masks        -- STA 
                   | StoreX a Masks        -- STX 
                   | StoreI1 a Masks       -- ST1 
                   | StoreI2 a Masks       -- ST2      
                   | StoreI3 a Masks       -- ST3 
                   | StoreI4 a Masks       -- ST4 
                   | StoreI5 a Masks       -- ST5 
                   | StoreI6 a Masks       -- ST6 
                   | StoreJ a Masks        -- STJ 
                   | StoreZero a Masks     -- STZ 
                   | Add a Masks           -- ADD 
                   | Sub a Masks           -- SUB 
                   | AddX Masks            -- ADDX
                   | SubX Masks            -- SUBX
                   | EnterA a              -- ENTA 
                   | EnterX a              -- ENTX 
                   | EnterI1 a             -- ENT1 
                   | EnterI2 a             -- ENT2 
                   | EnterI3 a             -- ENT3 
                   | EnterI4 a             -- ENT4 
                   | EnterI5 a             -- ENT5 
                   | EnterI6 a             -- ENT6 
                   | EnterANeg a           -- ENNA 
                   | EnterXNeg a           -- ENNX 
                   | EnterI1Neg a          -- ENN1 
                   | EnterI2Neg a          -- ENN2 
                   | EnterI3Neg a          -- ENN3 
                   | EnterI4Neg a          -- ENN4 
                   | EnterI5Neg a          -- ENN5 
                   | EnterI6Neg a          -- ENN6 
                   | IncrementA a          -- INCA 
                   | IncrementX a          -- INCX 
                   | IncrementI1 a         -- INC1 
                   | IncrementI2 a         -- INC2 
                   | IncrementI3 a         -- INC3 
                   | IncrementI4 a         -- INC4 
                   | IncrementI5 a         -- INC5 
                   | IncrementI6 a         -- INC6 
                   | DecrementA a          -- DECA 
                   | DecrementX a          -- DECX 
                   | DecrementI1 a         -- DEC1 
                   | DecrementI2 a         -- DEC2 
                   | DecrementI3 a         -- DEC3 
                   | DecrementI4 a         -- DEC4 
                   | DecrementI5 a         -- DEC5 
                   | DecrementI6 a         -- DEC6 
                   | CompareA a Masks      -- CMPA 
                   | CompareX a Masks      -- CMPX 
                   | CompareI1 a Masks     -- CMP1 
                   | CompareI2 a Masks     -- CMP2 
                   | CompareI3 a Masks     -- CMP3 
                   | CompareI4 a Masks     -- CMP4 
                   | CompareI5 a Masks     -- CMP5 
                   | CompareI6 a Masks     -- CMP6 
                   | Jump a                -- JMP 
                   | JumpSaveJ a           -- JSJ 
                   | JumpOnOverflow a      -- JOV 
                   | JumpOnNoOverflow a    -- JNOV 
                   | JumpOnLess a          -- JL 
                   | JumpOnEqual a         -- JE 
                   | JumpOnGreater a       -- JG 
                   | JumpOnGreaterEqual a  -- JGE 
                   | JumpOnUnEqual a       -- JNE 
                   | JumpOnLessEqual a     -- JLE 
                   | JumpANegative a       -- JAN 
                   | JumpAZero a           -- JAZ 
                   | JumpAPositive a       -- JAP 
                   | JumpANonNegative a    -- JANN 
                   | JumpANonZero a        -- JANZ 
                   | JumpANonPositive a    -- JANP 
                   | JumpXNegative a       -- JXN 
                   | JumpXZero a           -- JXZ 
                   | JumpXPositive a       -- JXP 
                   | JumpXNonNegative a    -- JXNN 
                   | JumpXNonZero a        -- JXNZ 
                   | JumpXNonPositive a    -- JXNP 
                   | JumpI1Negative a      -- J1N 
                   | JumpI1Zero a          -- J1Z 
                   | JumpI1Positive a      -- J1P 
                   | JumpI1NonNegative a   -- J1NN 
                   | JumpI1NonZero a       -- J1NZ 
                   | JumpI1NonPositive a   -- J1NP 
                   | JumpI2Negative a      -- J2N  
                   | JumpI2Zero a          -- J2Z 
                   | JumpI2Positive a      -- J2P 
                   | JumpI2NonNegative a   -- J2NN 
                   | JumpI2NonZero a       -- J2NZ 
                   | JumpI2NonPositive a   -- J2NP 
                   | JumpI3Negative a      -- J3N 
                   | JumpI3Zero a          -- J3Z 
                   | JumpI3Positive a      -- J3P 
                   | JumpI3NonNegative a   -- J3NN 
                   | JumpI3NonZero a       -- J3NZ 
                   | JumpI3NonPositive a   -- J3NP 
                   | JumpI4Negative a      -- J4N 
                   | JumpI4Zero a          -- J4Z 
                   | JumpI4Positive a      -- J4P 
                   | JumpI4NonNegative a   -- J4NN 
                   | JumpI4NonZero a       -- J4NZ 
                   | JumpI4NonPositive a   -- J4NP 
                   | JumpI5Negative a      -- J5N 
                   | JumpI5Zero a          -- J5Z 
                   | JumpI5Positive a      -- J5P 
                   | JumpI5NonNegative a   -- J5NN 
                   | JumpI5NonZero a       -- J5NZ 
                   | JumpI5NonPositive a   -- J5NP 
                   | JumpI6Negative a      -- J6N 
                   | JumpI6Zero a          -- J6Z 
                   | JumpI6Positive a      -- J6P 
                   | JumpI6NonNegative a   -- J6NN 
                   | JumpI6NonZero a       -- J6NZ 
                   | JumpI6NonPositive a   -- J6NP 
                   | ShiftA a              -- SA 
                   | ShiftX a              -- SX 
                   | ShiftACircular a      -- SAC 
                   | ShiftXCircular a      -- SAX 
                   | SwapAX                -- SWAP
                   | MoveXI1               -- MOVX1
                   | MoveXI2               -- MOVX2
                   | MoveXI3               -- MOVX3
                   | MoveXI4               -- MOVX4
                   | MoveXI5               -- MOVX5
                   | MoveXI6               -- MOVX6
                   | NoOperation           -- NOP
                   | Halt                  -- HLT


distributeResult : Instruction ( Result e a ) -> Result e ( Instruction a )
distributeResult i =
    case i of
       LoadA r m ->      case r of
                             Err err -> Err err
                             Ok x -> Ok <| LoadA x m
       LoadX r m ->      case r of
                             Err err -> Err err
                             Ok x -> Ok <| LoadX x m                 
       LoadI1 r m ->     case r of
                             Err err -> Err err
                             Ok x -> Ok <| LoadI1 x m
       LoadI2 r m ->     case r of
                             Err err -> Err err
                             Ok x -> Ok <| LoadI2 x m              
       LoadI3 r m ->     case r of
                             Err err -> Err err
                             Ok x -> Ok <| LoadI3 x m              
       LoadI4 r m ->     case r of
                             Err err -> Err err
                             Ok x -> Ok <|  LoadI4 x m             
       LoadI5 r m ->     case r of
                             Err err -> Err err
                             Ok x -> Ok <|  LoadI5 x m             
       LoadI6 r m ->     case r of
                             Err err -> Err err
                             Ok x -> Ok <|  LoadI6 x m              
       LoadANeg r m ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  LoadANeg x m           
       LoadXNeg r m ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <| LoadXNeg x m            
       LoadI1Neg r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| LoadI1Neg x m            
       LoadI2Neg r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  LoadI2Neg x m           
       LoadI3Neg r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  LoadI3Neg x m           
       LoadI4Neg r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  LoadI4Neg x m           
       LoadI5Neg r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  LoadI5Neg x m 
       LoadI6Neg r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  LoadI6Neg x m 
       StoreA r m ->     case r of
                             Err err -> Err err
                             Ok x -> Ok <|     StoreA x m 
       StoreX r m ->     case r of
                             Err err -> Err err
                             Ok x -> Ok <|     StoreX x m
       StoreI1 r m ->    case r of
                             Err err -> Err err
                             Ok x -> Ok <|    StoreI1 x m
       StoreI2 r m ->    case r of
                             Err err -> Err err
                             Ok x -> Ok <|    StoreI2 x m
       StoreI3 r m ->    case r of
                             Err err -> Err err
                             Ok x -> Ok <|    StoreI3 x m
       StoreI4 r m ->    case r of
                             Err err -> Err err
                             Ok x -> Ok <|    StoreI4 x m
       StoreI5 r m ->    case r of
                             Err err -> Err err
                             Ok x -> Ok <|    StoreI5 x m
       StoreI6 r m ->    case r of
                             Err err -> Err err
                             Ok x -> Ok <|    StoreI6 x m
       StoreJ r m ->     case r of
                             Err err -> Err err
                             Ok x -> Ok <|     StoreJ x m
       StoreZero r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  StoreZero x m
       Add r m ->        case r of
                             Err err -> Err err
                             Ok x -> Ok <| Add x m
       Sub r m ->        case r of
                             Err err -> Err err
                             Ok x -> Ok <| Sub x m 
       AddX m ->         Ok <| AddX m
       SubX m ->         Ok <| SubX m
       EnterA r ->       case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterA x
       EnterX r ->       case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterX x
       EnterI1 r ->      case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterI1 x
       EnterI2 r ->      case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterI2 x
       EnterI3 r ->      case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterI3 x
       EnterI4 r ->      case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterI4 x
       EnterI5 r ->      case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterI5 x
       EnterI6 r ->      case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterI6 x
       EnterANeg r ->    case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterANeg x
       EnterXNeg r ->    case r of
                             Err err -> Err err
                             Ok x -> Ok <| EnterXNeg x
       EnterI1Neg r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  EnterI1Neg x
       EnterI2Neg r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  EnterI2Neg x
       EnterI3Neg r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  EnterI3Neg x
       EnterI4Neg r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  EnterI4Neg x
       EnterI5Neg r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  EnterI5Neg x
       EnterI6Neg r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  EnterI6Neg x
       IncrementA r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  IncrementA x
       IncrementX r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  IncrementX x
       IncrementI1 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| IncrementI1 x
       IncrementI2 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| IncrementI2 x
       IncrementI3 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|    IncrementI3 x
       IncrementI4 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| IncrementI4 x
       IncrementI5 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| IncrementI5 x
       IncrementI6 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| IncrementI6 x
       DecrementA r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <| DecrementA x
       DecrementX r ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <|  DecrementX x
       DecrementI1 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| DecrementI1 x
       DecrementI2 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| DecrementI2 x
       DecrementI3 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| DecrementI3 x
       DecrementI4 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| DecrementI4 x
       DecrementI5 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| DecrementI5 x
       DecrementI6 r ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| DecrementI6 x
       CompareA r m ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <| CompareA x m
       CompareX r m ->   case r of
                             Err err -> Err err
                             Ok x -> Ok <| CompareX x m
       CompareI1 r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <| CompareI1 x m
       CompareI2 r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  CompareI2 x m
       CompareI3 r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  CompareI3 x m
       CompareI4 r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  CompareI4 x m
       CompareI5 r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  CompareI5 x m
       CompareI6 r m ->  case r of
                             Err err -> Err err
                             Ok x -> Ok <|  CompareI6 x m
       Jump r ->         case r of
                             Err err -> Err err
                             Ok x -> Ok <| Jump x
       JumpSaveJ r ->    case r of
                             Err err -> Err err
                             Ok x -> Ok <| JumpSaveJ x
       JumpOnOverflow r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpOnOverflow x
       JumpOnNoOverflow r ->    case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|  JumpOnNoOverflow x
       JumpOnLess r ->          case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|   JumpOnLess x
       JumpOnEqual r ->         case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|  JumpOnEqual x
       JumpOnGreater r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpOnGreater x
       JumpOnGreaterEqual r ->  case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpOnGreaterEqual x 
       JumpOnUnEqual r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpOnUnEqual x
       JumpOnLessEqual r ->     case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpOnLessEqual x
       JumpANegative r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpANegative x
       JumpAZero r ->           case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpAZero x
       JumpAPositive r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpAPositive x 
       JumpANonNegative r ->    case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|  JumpANonNegative x
       JumpANonZero r ->        case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpANonZero x
       JumpANonPositive r ->    case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|  JumpANonPositive x
       JumpXNegative r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpXNegative x
       JumpXZero r ->           case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpXZero x
       JumpXPositive r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpXPositive x
       JumpXNonNegative r ->    case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|  JumpXNonNegative x
       JumpXNonZero r ->        case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpXNonZero x
       JumpXNonPositive r ->    case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|  JumpXNonPositive x
       JumpI1Negative r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI1Negative x
       JumpI1Zero r ->          case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|   JumpI1Zero x
       JumpI1Positive r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI1Positive x
       JumpI1NonNegative r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI1NonNegative x
       JumpI1NonZero r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI1NonZero x
       JumpI1NonPositive r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI1NonPositive x
       JumpI2Negative r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI2Negative x
       JumpI2Zero r ->          case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|   JumpI2Zero x
       JumpI2Positive r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI2Positive x
       JumpI2NonNegative r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI2NonNegative x
       JumpI2NonZero r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI2NonZero x
       JumpI2NonPositive r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI2NonPositive x
       JumpI3Negative r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI3Negative x
       JumpI3Zero r ->          case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|   JumpI3Zero x
       JumpI3Positive r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI3Positive x
       JumpI3NonNegative r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI3NonNegative x
       JumpI3NonZero r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI3NonZero x
       JumpI3NonPositive r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI3NonPositive x
       JumpI4Negative r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI4Negative x
       JumpI4Zero r ->          case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|   JumpI4Zero x
       JumpI4Positive r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI4Positive x
       JumpI4NonNegative r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI4NonNegative x
       JumpI4NonZero r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI4NonZero x 
       JumpI4NonPositive r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI4NonPositive x
       JumpI5Negative r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI5Negative x
       JumpI5Zero r ->          case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|   JumpI5Zero x
       JumpI5Positive r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI5Positive x
       JumpI5NonNegative r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI5NonNegative x
       JumpI5NonZero r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI5NonZero x
       JumpI5NonPositive r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI5NonPositive x
       JumpI6Negative r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI6Negative x
       JumpI6Zero r ->          case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|   JumpI6Zero x
       JumpI6Positive r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    JumpI6Positive x
       JumpI6NonNegative r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI6NonNegative x
       JumpI6NonZero r ->       case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|  JumpI6NonZero x
       JumpI6NonPositive r ->   case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| JumpI6NonPositive x
       ShiftA r ->              case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|       ShiftA x
       ShiftX r ->              case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|       ShiftX x
       ShiftACircular r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <|    ShiftACircular x
       ShiftXCircular r ->      case r of
                                    Err err -> Err err
                                    Ok x -> Ok <| ShiftXCircular x
       SwapAX ->             Ok SwapAX
       MoveXI1 ->            Ok MoveXI1
       MoveXI2 ->            Ok MoveXI2
       MoveXI3 ->            Ok MoveXI3
       MoveXI4 ->            Ok MoveXI4
       MoveXI5 ->            Ok MoveXI5
       MoveXI6 ->            Ok MoveXI6
       NoOperation ->        Ok NoOperation
       Halt        ->        Ok Halt



mapInstruction : (a -> b) -> Instruction a -> Instruction b
mapInstruction f i =
    case i of
       LoadA a m ->               LoadA ( f a ) m                
       LoadX a m ->               LoadX ( f a ) m                
       LoadI1 a m ->              LoadI1 ( f a ) m               
       LoadI2 a m ->              LoadI2 ( f a ) m               
       LoadI3 a m ->              LoadI3 ( f a ) m               
       LoadI4 a m ->              LoadI4 ( f a ) m               
       LoadI5 a m ->              LoadI5 ( f a ) m               
       LoadI6 a m ->              LoadI6 ( f a ) m               
       LoadANeg a m ->            LoadANeg ( f a ) m             
       LoadXNeg a m ->            LoadXNeg ( f a ) m             
       LoadI1Neg a m ->           LoadI1Neg ( f a ) m            
       LoadI2Neg a m ->           LoadI2Neg ( f a ) m            
       LoadI3Neg a m ->           LoadI3Neg ( f a ) m            
       LoadI4Neg a m ->           LoadI4Neg ( f a ) m            
       LoadI5Neg a m ->           LoadI5Neg ( f a ) m            
       LoadI6Neg a m ->           LoadI6Neg ( f a ) m            
       StoreA a m ->              StoreA ( f a ) m               
       StoreX a m ->              StoreX ( f a ) m               
       StoreI1 a m ->             StoreI1 ( f a ) m              
       StoreI2 a m ->             StoreI2 ( f a ) m              
       StoreI3 a m ->             StoreI3 ( f a ) m              
       StoreI4 a m ->             StoreI4 ( f a ) m              
       StoreI5 a m ->             StoreI5 ( f a ) m              
       StoreI6 a m ->             StoreI6 ( f a ) m              
       StoreJ a m ->              StoreJ ( f a ) m               
       StoreZero a m ->           StoreZero ( f a ) m            
       Add a m ->                 Add ( f a ) m                  
       Sub a m ->                 Sub ( f a ) m                  
       AddX m ->                  AddX m                   
       SubX m ->                  SubX m                   
       EnterA a ->                EnterA ( f a )                
       EnterX a ->                EnterX ( f a )                
       EnterI1 a ->               EnterI1 ( f a )               
       EnterI2 a ->               EnterI2 ( f a )               
       EnterI3 a ->               EnterI3 ( f a )               
       EnterI4 a ->               EnterI4 ( f a )               
       EnterI5 a ->               EnterI5 ( f a )               
       EnterI6 a ->               EnterI6 ( f a )               
       EnterANeg a ->             EnterANeg ( f a )             
       EnterXNeg a ->             EnterXNeg ( f a )             
       EnterI1Neg a ->            EnterI1Neg ( f a )            
       EnterI2Neg a ->            EnterI2Neg ( f a )            
       EnterI3Neg a ->            EnterI3Neg ( f a )            
       EnterI4Neg a ->            EnterI4Neg ( f a )            
       EnterI5Neg a ->            EnterI5Neg ( f a )            
       EnterI6Neg a ->            EnterI6Neg ( f a )            
       IncrementA a ->            IncrementA ( f a )            
       IncrementX a ->            IncrementX ( f a )            
       IncrementI1 a ->           IncrementI1 ( f a )           
       IncrementI2 a ->           IncrementI2 ( f a )          
       IncrementI3 a ->           IncrementI3 ( f a )         
       IncrementI4 a ->           IncrementI4 ( f a )           
       IncrementI5 a ->           IncrementI5 ( f a )           
       IncrementI6 a ->           IncrementI6 ( f a )           
       DecrementA a ->            DecrementA ( f a )            
       DecrementX a ->            DecrementX ( f a )            
       DecrementI1 a ->           DecrementI1 ( f a )           
       DecrementI2 a ->           DecrementI2 ( f a )           
       DecrementI3 a ->           DecrementI3 ( f a )           
       DecrementI4 a ->           DecrementI4 ( f a )           
       DecrementI5 a ->           DecrementI5 ( f a )           
       DecrementI6 a ->           DecrementI6 ( f a )           
       CompareA a m ->            CompareA ( f a ) m             
       CompareX a m ->            CompareX ( f a ) m             
       CompareI1 a m ->           CompareI1 ( f a ) m            
       CompareI2 a m ->           CompareI2 ( f a ) m            
       CompareI3 a m ->           CompareI3 ( f a ) m            
       CompareI4 a m ->           CompareI4 ( f a ) m            
       CompareI5 a m ->           CompareI5 ( f a ) m            
       CompareI6 a m ->           CompareI6 ( f a ) m            
       Jump a ->                  Jump ( f a )                  
       JumpSaveJ a ->             JumpSaveJ ( f a )             
       JumpOnOverflow a ->        JumpOnOverflow ( f a )        
       JumpOnNoOverflow a ->      JumpOnNoOverflow ( f a )      
       JumpOnLess a ->            JumpOnLess ( f a )            
       JumpOnEqual a ->           JumpOnEqual ( f a )           
       JumpOnGreater a ->         JumpOnGreater ( f a )         
       JumpOnGreaterEqual a ->    JumpOnGreaterEqual ( f a )    
       JumpOnUnEqual a ->         JumpOnUnEqual ( f a )         
       JumpOnLessEqual a ->       JumpOnLessEqual ( f a )       
       JumpANegative a ->         JumpANegative ( f a )         
       JumpAZero a ->             JumpAZero ( f a )             
       JumpAPositive a ->         JumpAPositive ( f a )         
       JumpANonNegative a ->      JumpANonNegative ( f a )      
       JumpANonZero a ->          JumpANonZero ( f a )          
       JumpANonPositive a ->      JumpANonPositive ( f a )      
       JumpXNegative a ->         JumpXNegative ( f a )         
       JumpXZero a ->             JumpXZero ( f a )             
       JumpXPositive a ->         JumpXPositive ( f a )         
       JumpXNonNegative a ->      JumpXNonNegative ( f a )      
       JumpXNonZero a ->          JumpXNonZero ( f a )          
       JumpXNonPositive a ->      JumpXNonPositive ( f a )      
       JumpI1Negative a ->        JumpI1Negative ( f a )        
       JumpI1Zero a ->            JumpI1Zero ( f a )            
       JumpI1Positive a ->        JumpI1Positive ( f a )        
       JumpI1NonNegative a ->     JumpI1NonNegative ( f a )     
       JumpI1NonZero a ->         JumpI1NonZero ( f a )         
       JumpI1NonPositive a ->     JumpI1NonPositive ( f a )     
       JumpI2Negative a ->        JumpI2Negative ( f a )        
       JumpI2Zero a ->            JumpI2Zero ( f a )            
       JumpI2Positive a ->        JumpI2Positive ( f a )        
       JumpI2NonNegative a ->     JumpI2NonNegative ( f a )     
       JumpI2NonZero a ->         JumpI2NonZero ( f a )         
       JumpI2NonPositive a ->     JumpI2NonPositive ( f a )     
       JumpI3Negative a ->        JumpI3Negative ( f a )        
       JumpI3Zero a ->            JumpI3Zero ( f a )            
       JumpI3Positive a ->        JumpI3Positive ( f a )        
       JumpI3NonNegative a ->     JumpI3NonNegative ( f a )     
       JumpI3NonZero a ->         JumpI3NonZero ( f a )         
       JumpI3NonPositive a ->     JumpI3NonPositive ( f a )     
       JumpI4Negative a ->        JumpI4Negative ( f a )        
       JumpI4Zero a ->            JumpI4Zero ( f a )            
       JumpI4Positive a ->        JumpI4Positive ( f a )        
       JumpI4NonNegative a ->     JumpI4NonNegative ( f a )     
       JumpI4NonZero a ->         JumpI4NonZero ( f a )         
       JumpI4NonPositive a ->     JumpI4NonPositive ( f a )     
       JumpI5Negative a ->        JumpI5Negative ( f a )        
       JumpI5Zero a ->            JumpI5Zero ( f a )            
       JumpI5Positive a ->        JumpI5Positive ( f a )        
       JumpI5NonNegative a ->     JumpI5NonNegative ( f a )     
       JumpI5NonZero a ->         JumpI5NonZero ( f a )         
       JumpI5NonPositive a ->     JumpI5NonPositive ( f a )     
       JumpI6Negative a ->        JumpI6Negative ( f a )        
       JumpI6Zero a ->            JumpI6Zero ( f a )            
       JumpI6Positive a ->        JumpI6Positive ( f a )        
       JumpI6NonNegative a ->     JumpI6NonNegative ( f a )     
       JumpI6NonZero a ->         JumpI6NonZero ( f a )         
       JumpI6NonPositive a ->     JumpI6NonPositive ( f a )     
       ShiftA a ->                ShiftA ( f a )                
       ShiftX a ->                ShiftX ( f a )                
       ShiftACircular a ->        ShiftACircular ( f a )        
       ShiftXCircular a ->        ShiftXCircular ( f a )        
       SwapAX ->                  SwapAX                   
       MoveXI1 ->                 MoveXI1                  
       MoveXI2 ->                 MoveXI2                  
       MoveXI3 ->                 MoveXI3                  
       MoveXI4 ->                 MoveXI4                  
       MoveXI5 ->                 MoveXI5                  
       MoveXI6 ->                 MoveXI6                  
       NoOperation ->             NoOperation              
       Halt        ->             Halt                     


type CompileTimeError = InvalidModification Modification
                      | UnrecognizedInstructionCode InstructionCode 
                                  
decodeInstruction : UnpackedWord -> Result CompileTimeError StaticInstruction
decodeInstruction (a,i,f,ms,c) =
    case c of
        0  -> Ok NoOperation
        8  -> Ok <| LoadA (i,a) ms
        15 -> Ok <| LoadX (i,a) ms
        9  -> Ok <| LoadI1 (i,a) ms
        10 -> Ok <| LoadI2 (i,a) ms
        11 -> Ok <| LoadI3 (i,a) ms
        12 -> Ok <| LoadI4 (i,a) ms
        13 -> Ok <| LoadI5 (i,a) ms
        14 -> Ok <| LoadI6 (i,a) ms
        16 -> Ok <| LoadANeg (i,a) ms
        23 -> Ok <| LoadXNeg (i,a) ms
        17 -> Ok <| LoadI1Neg (i,a) ms
        18 -> Ok <| LoadI2Neg (i,a) ms
        19 -> Ok <| LoadI3Neg (i,a) ms
        20 -> Ok <| LoadI4Neg (i,a) ms
        21 -> Ok <| LoadI5Neg (i,a) ms
        22 -> Ok <| LoadI6Neg (i,a) ms
        24 -> Ok <| StoreA (i,a) ms
        31 -> Ok <| StoreX (i,a) ms
        25 -> Ok <| StoreI1 (i,a) ms
        26 -> Ok <| StoreI2 (i,a) ms
        27 -> Ok <| StoreI3 (i,a) ms
        28 -> Ok <| StoreI4 (i,a) ms
        29 -> Ok <| StoreI5 (i,a) ms
        30 -> Ok <| StoreI6 (i,a) ms
        32 -> Ok <| StoreJ (i,a) ms
        33 -> Ok <| StoreZero (i,a) ms
        1  -> Ok <| Add (i,a) ms
        2  -> Ok <| Sub (i,a) ms
        3  -> Ok <| AddX ms
        4  -> Ok <| SubX ms
        56 -> Ok <| CompareA (i,a) ms
        63 -> Ok <| CompareX (i,a) ms
        57 -> Ok <| CompareI1 (i,a) ms
        58 -> Ok <| CompareI2 (i,a) ms
        59 -> Ok <| CompareI3 (i,a) ms
        60 -> Ok <| CompareI4 (i,a) ms
        61 -> Ok <| CompareI5 (i,a) ms
        62 -> Ok <| CompareI6 (i,a) ms
        48 -> case f of
                  2 -> Ok <| EnterA (i,a)
                  3 -> Ok <| EnterANeg (i,a)
                  0 -> Ok <| IncrementA (i,a)
                  1 -> Ok <| DecrementA (i,a)
                  y -> Err <| InvalidModification f
        55 -> case f of
                  2 -> Ok <| EnterX (i,a)
                  3 -> Ok <| EnterXNeg (i,a)
                  0 -> Ok <| IncrementX (i,a)
                  1 -> Ok <| DecrementX (i,a)
                  y -> Err <| InvalidModification f
        49 -> case f of
                  2 -> Ok <| EnterI1 (i,a)
                  3 -> Ok <| EnterI1Neg (i,a)
                  0 -> Ok <| IncrementI1 (i,a)
                  1 -> Ok <| DecrementI1 (i,a)
                  y -> Err <| InvalidModification f
        50 -> case f of
                  2 -> Ok <| EnterI2 (i,a)
                  3 -> Ok <| EnterI2Neg (i,a)
                  0 -> Ok <| IncrementI2 (i,a)
                  1 -> Ok <| DecrementI2 (i,a)
                  y -> Err <| InvalidModification f
        51 -> case f of
                  2 -> Ok <| EnterI3 (i,a)
                  3 -> Ok <| EnterI3Neg (i,a)
                  0 -> Ok <| IncrementI3 (i,a)
                  1 -> Ok <| DecrementI3 (i,a)
                  y -> Err <| InvalidModification f
        52 -> case f of
                  2 -> Ok <| EnterI4 (i,a)
                  3 -> Ok <| EnterI4Neg (i,a)
                  0 -> Ok <| IncrementI4 (i,a)
                  1 -> Ok <| DecrementI4 (i,a)
                  y -> Err <| InvalidModification f
        53 -> case f of
                  2 -> Ok <| EnterI5 (i,a)
                  3 -> Ok <| EnterI5Neg (i,a)
                  0 -> Ok <| IncrementI5 (i,a)
                  1 -> Ok <| DecrementI5 (i,a)
                  y -> Err <| InvalidModification f
        54 -> case f of
                  2 -> Ok <| EnterI6 (i,a)
                  3 -> Ok <| EnterI6Neg (i,a)
                  0 -> Ok <| IncrementI6 (i,a)
                  1 -> Ok <| DecrementI6 (i,a)
                  y -> Err <| InvalidModification f
        39 -> case f of
                  0 -> Ok <| Jump (i,a)
                  1 -> Ok <| JumpSaveJ (i,a)
                  2 -> Ok <| JumpOnOverflow (i,a)
                  3 -> Ok <| JumpOnNoOverflow (i,a)
                  4 -> Ok <| JumpOnLess (i,a)
                  5 -> Ok <| JumpOnEqual (i,a)
                  6 -> Ok <| JumpOnGreater (i,a)
                  7 -> Ok <| JumpOnGreaterEqual (i,a)
                  8 -> Ok <| JumpOnUnEqual (i,a)
                  9 -> Ok <| JumpOnLessEqual (i,a)
                  y -> Err <| InvalidModification f
        40 -> case f of
                  0 -> Ok <| JumpANegative (i,a)
                  1 -> Ok <| JumpAZero (i,a)
                  2 -> Ok <| JumpAPositive (i,a)
                  3 -> Ok <| JumpANonNegative (i,a)
                  4 -> Ok <| JumpANonZero (i,a)
                  5 -> Ok <| JumpANonPositive (i,a)
                  y -> Err <| InvalidModification f
        47 -> case f of
                  0 -> Ok <| JumpXNegative (i,a)
                  1 -> Ok <| JumpXZero (i,a)
                  2 -> Ok <| JumpXPositive (i,a)
                  3 -> Ok <| JumpXNonNegative (i,a)
                  4 -> Ok <| JumpXNonZero (i,a)
                  5 -> Ok <| JumpXNonPositive (i,a)
                  y -> Err <| InvalidModification f
        41 -> case f of
                  0 -> Ok <| JumpI1Negative (i,a)
                  1 -> Ok <| JumpI1Zero (i,a)
                  2 -> Ok <| JumpI1Positive (i,a)
                  3 -> Ok <| JumpI1NonNegative (i,a)
                  4 -> Ok <| JumpI1NonZero (i,a)
                  5 -> Ok <| JumpI1NonPositive (i,a)
                  y -> Err <| InvalidModification f
        42 -> case f of
                  0 -> Ok <| JumpI2Negative (i,a)
                  1 -> Ok <| JumpI2Zero (i,a)
                  2 -> Ok <| JumpI2Positive (i,a)
                  3 -> Ok <| JumpI2NonNegative (i,a)
                  4 -> Ok <| JumpI2NonZero (i,a)
                  5 -> Ok <| JumpI2NonPositive (i,a)
                  y -> Err <| InvalidModification f
        43 -> case f of
                  0 -> Ok <| JumpI3Negative (i,a)
                  1 -> Ok <| JumpI3Zero (i,a)
                  2 -> Ok <| JumpI3Positive (i,a)
                  3 -> Ok <| JumpI3NonNegative (i,a)
                  4 -> Ok <| JumpI3NonZero (i,a)
                  5 -> Ok <| JumpI3NonPositive (i,a)
                  y -> Err <| InvalidModification f
        44 -> case f of
                  0 -> Ok <| JumpI4Negative (i,a)
                  1 -> Ok <| JumpI4Zero (i,a)
                  2 -> Ok <| JumpI4Positive (i,a)
                  3 -> Ok <| JumpI4NonNegative (i,a)
                  4 -> Ok <| JumpI4NonZero (i,a)
                  5 -> Ok <| JumpI4NonPositive (i,a)
                  y -> Err <| InvalidModification f
        45 -> case f of
                  0 -> Ok <| JumpI5Negative (i,a)
                  1 -> Ok <| JumpI5Zero (i,a)
                  2 -> Ok <| JumpI5Positive (i,a)
                  3 -> Ok <| JumpI5NonNegative (i,a)
                  4 -> Ok <| JumpI5NonZero (i,a)
                  5 -> Ok <| JumpI5NonPositive (i,a)
                  y -> Err <| InvalidModification f
        46 -> case f of
                  0 -> Ok <| JumpI6Negative (i,a)
                  1 -> Ok <| JumpI6Zero (i,a)
                  2 -> Ok <| JumpI6Positive (i,a)
                  3 -> Ok <| JumpI6NonNegative (i,a)
                  4 -> Ok <| JumpI6NonZero (i,a)
                  5 -> Ok <| JumpI6NonPositive (i,a)
                  y -> Err <| InvalidModification f
        6  -> case f of
                  0 -> Ok <| ShiftA (i,a)
                  1 -> Ok <| ShiftX (i,a)
                  2 -> Ok <| ShiftACircular (i,a)
                  3 -> Ok <| ShiftXCircular (i,a)
                  4 -> Ok SwapAX
                  y -> Err <| InvalidModification f
        7  -> case f of
                  0 -> Ok MoveXI1
                  1 -> Ok MoveXI2
                  2 -> Ok MoveXI3
                  3 -> Ok MoveXI4
                  4 -> Ok MoveXI5
                  5 -> Ok MoveXI6
                  y -> Err <| InvalidModification f
        5  -> case f of
                  2 -> Ok Halt
                  y -> Err <| InvalidModification f
        x  -> Err <| UnrecognizedInstructionCode x


packWithMask : (Address,Index) -> Masks -> InstructionCode -> Word
packWithMask (a,i) ms c = let (t,(s,b1,b2)) = intToSmallWord a zeroSmallWord
                        in (s,b1,b2,byte 3,masksToByte ms,byte c)

packWithMod : (Address,Index) -> Modification -> InstructionCode -> Word
packWithMod (a,i) m c = let (t,(s,b1,b2)) = intToSmallWord a zeroSmallWord
                      in (s,b1,b2,byte 3,byte m,byte c)




encodeInstruction : StaticInstruction -> Word
encodeInstruction i =
    case i of
       LoadA a m -> packWithMask a m 8
       LoadX a m -> packWithMask a m 15          
       LoadI1 a m -> packWithMask a m 9          
       LoadI2 a m -> packWithMask a m 10           
       LoadI3 a m -> packWithMask a m 11          
       LoadI4 a m -> packWithMask a m 12          
       LoadI5 a m -> packWithMask a m 13          
       LoadI6 a m -> packWithMask a m 14          
       LoadANeg a m -> packWithMask a m 16          
       LoadXNeg a m -> packWithMask a m 23        
       LoadI1Neg a m -> packWithMask a m 17        
       LoadI2Neg a m -> packWithMask a m 18       
       LoadI3Neg a m -> packWithMask a m 19       
       LoadI4Neg a m -> packWithMask a m 20       
       LoadI5Neg a m -> packWithMask a m 21       
       LoadI6Neg a m -> packWithMask a m 22       
       StoreA a m -> packWithMask a m 24       
       StoreX a m -> packWithMask a m 31          
       StoreI1 a m -> packWithMask a m 25          
       StoreI2 a m -> packWithMask a m 26         
       StoreI3 a m -> packWithMask a m 27         
       StoreI4 a m -> packWithMask a m 28         
       StoreI5 a m -> packWithMask a m 29         
       StoreI6 a m -> packWithMask a m 30         
       StoreJ a m -> packWithMask a m 32         
       StoreZero a m -> packWithMask a m 33         
       Add a m -> packWithMask a m 1       
       Sub a m -> packWithMask a m 2              
       AddX m -> packWithMask (0,0) m 3
       SubX m -> packWithMask (0,0) m 4
       EnterA a -> packWithMod a 2 48
       EnterX a -> packWithMod a 2 55          
       EnterI1 a -> packWithMod a 2 49            
       EnterI2 a -> packWithMod a 2 50             
       EnterI3 a -> packWithMod a 2 51             
       EnterI4 a -> packWithMod a 2 52         
       EnterI5 a -> packWithMod a 2 53    
       EnterI6 a -> packWithMod a 2 54         
       EnterANeg a -> packWithMod a 3 48         
       EnterXNeg a -> packWithMod a 3 55       
       EnterI1Neg a -> packWithMod a 3 49       
       EnterI2Neg a -> packWithMod a 3 50      
       EnterI3Neg a -> packWithMod a 3 51      
       EnterI4Neg a -> packWithMod a 3 52      
       EnterI5Neg a -> packWithMod a 3 53      
       EnterI6Neg a -> packWithMod a 3 54      
       IncrementA a -> packWithMod a 0 48      
       IncrementX a -> packWithMod a 0 55      
       IncrementI1 a -> packWithMod a 0 49      
       IncrementI2 a -> packWithMod a 0 50     
       IncrementI3 a -> packWithMod a 0 51     
       IncrementI4 a -> packWithMod a 0 52     
       IncrementI5 a -> packWithMod a 0 53     
       IncrementI6 a -> packWithMod a 0 54     
       DecrementA a -> packWithMod a 1 48     
       DecrementX a -> packWithMod a 1 55      
       DecrementI1 a -> packWithMod a 1 49      
       DecrementI2 a -> packWithMod a 1 50     
       DecrementI3 a -> packWithMod a 1 51     
       DecrementI4 a -> packWithMod a 1 52     
       DecrementI5 a -> packWithMod a 1 53     
       DecrementI6 a -> packWithMod a 1 54     
       CompareA a m -> packWithMask a m 56          
       CompareX a m -> packWithMask a m 63        
       CompareI1 a m -> packWithMask a m 57       
       CompareI2 a m -> packWithMask a m 58       
       CompareI3 a m -> packWithMask a m 59       
       CompareI4 a m -> packWithMask a m 60       
       CompareI5 a m -> packWithMask a m 61       
       CompareI6 a m -> packWithMask a m 62       
       Jump a -> packWithMod a 0 39    
       JumpSaveJ a -> packWithMod a 1 39           
       JumpOnOverflow a -> packWithMod a 2 39      
       JumpOnNoOverflow a -> packWithMod a 3 39  
       JumpOnLess a -> packWithMod a 4 39
       JumpOnEqual a -> packWithMod a 5 39      
       JumpOnGreater a -> packWithMod a 6 39     
       JumpOnGreaterEqual a -> packWithMod a 7 39  
       JumpOnUnEqual a -> packWithMod a 8 39
       JumpOnLessEqual a -> packWithMod a 9 39   
       JumpANegative a -> packWithMod a 0 40 
       JumpAZero a -> packWithMod a 1 40   
       JumpAPositive a -> packWithMod a 2 40
       JumpANonNegative a -> packWithMod a 3 40   
       JumpANonZero a -> packWithMod a 4 40
       JumpANonPositive a -> packWithMod a 5 40    
       JumpXNegative a -> packWithMod a 0 47
       JumpXZero a -> packWithMod a 1 47   
       JumpXPositive a -> packWithMod a 2 47       
       JumpXNonNegative a -> packWithMod a 3 47   
       JumpXNonZero a -> packWithMod a 4 47
       JumpXNonPositive a -> packWithMod a 5 47    
       JumpI1Negative a -> packWithMod a 0 41
       JumpI1Zero a -> packWithMod a 1 41  
       JumpI1Positive a -> packWithMod a 2 41      
       JumpI1NonNegative a -> packWithMod a 3 41  
       JumpI1NonZero a -> packWithMod a 4 41
       JumpI1NonPositive a -> packWithMod a 5 41   
       JumpI2Negative a -> packWithMod a 0 42
       JumpI2Zero a -> packWithMod a 1 42  
       JumpI2Positive a -> packWithMod a 2 42      
       JumpI2NonNegative a -> packWithMod a 3 42  
       JumpI2NonZero a -> packWithMod a 4 42
       JumpI2NonPositive a -> packWithMod a 5 42   
       JumpI3Negative a -> packWithMod a 0 43
       JumpI3Zero a -> packWithMod a 1 43  
       JumpI3Positive a -> packWithMod a 2 43      
       JumpI3NonNegative a -> packWithMod a 3 43  
       JumpI3NonZero a -> packWithMod a 4 43
       JumpI3NonPositive a -> packWithMod a 5 43   
       JumpI4Negative a -> packWithMod a 0 44
       JumpI4Zero a -> packWithMod a 1 44  
       JumpI4Positive a -> packWithMod a 2 44      
       JumpI4NonNegative a -> packWithMod a 3 44  
       JumpI4NonZero a -> packWithMod a 4 44
       JumpI4NonPositive a -> packWithMod a 5 44   
       JumpI5Negative a -> packWithMod a 0 45
       JumpI5Zero a -> packWithMod a 1 45  
       JumpI5Positive a -> packWithMod a 2 45      
       JumpI5NonNegative a -> packWithMod a 3 45  
       JumpI5NonZero a -> packWithMod a 4 45
       JumpI5NonPositive a -> packWithMod a 5 45   
       JumpI6Negative a -> packWithMod a 0 46
       JumpI6Zero a -> packWithMod a 1 46  
       JumpI6Positive a -> packWithMod a 2 46      
       JumpI6NonNegative a -> packWithMod a 3 46  
       JumpI6NonZero a -> packWithMod a 4 46
       JumpI6NonPositive a -> packWithMod a 5 46   
       ShiftA a -> packWithMod a 0 6
       ShiftX a -> packWithMod a 1 6           
       ShiftACircular a -> packWithMod a 2 6       
       ShiftXCircular a -> packWithMod a 3 6   
       SwapAX -> packWithMod (0,0) 4 6
       MoveXI1 -> packWithMod (0,0) 0 7              
       MoveXI2 -> packWithMod (0,0) 1 7             
       MoveXI3 -> packWithMod (0,0) 2 7             
       MoveXI4 -> packWithMod (0,0) 3 7             
       MoveXI5 -> packWithMod (0,0) 4 7             
       MoveXI6 -> packWithMod (0,0) 5 7             
       NoOperation -> packWithMod (0,0) 0 0  
       Halt        ->  packWithMod (0,0) 2 5

