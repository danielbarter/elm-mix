module Instructions exposing ( Instruction(..)
                             , mapInstruction
                             , unpack
                             , Modification
                             , InstructionCode
                             , Address
                             , decodeInstruction
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

type Instruction a = LoadA a Masks         -- LDA 
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

{-
distributeResult : Instruction ( Result e a ) -> Result e ( Instruction a )
distributeResult i =
    case i of
       LoadA r m ->      Just r                
       LoadX r m ->      Just r                 
       LoadI1 r m ->     Just r               
       LoadI2 r m ->     Just r               
       LoadI3 r m ->     Just r               
       LoadI4 r m ->     Just r               
       LoadI5 r m ->     Just r               
       LoadI6 r m ->     Just r               
       LoadANeg r m ->   Just r             
       LoadXNeg r m ->   Just r             
       LoadI1Neg r m ->  Just r            
       LoadI2Neg r m ->  Just r            
       LoadI3Neg r m ->  Just r            
       LoadI4Neg r m ->  Just r            
       LoadI5Neg r m ->  Just r            
       LoadI6Neg r m ->  Just r            
       StoreA r m ->     Just r               
       StoreX r m ->     Just r               
       StoreI1 r m ->    Just r              
       StoreI2 r m ->    Just r              
       StoreI3 r m ->    Just r              
       StoreI4 r m ->    Just r              
       StoreI5 r m ->    Just r              
       StoreI6 r m ->    Just r              
       StoreJ r m ->     Just r               
       StoreZero r m ->  Just r            
       Add r m ->        Just r                  
       Sub r m ->        Just r                  
       AddX m ->         Nothing     
       SubX m ->         Nothing
       EnterA r ->       Just r                
       EnterX r ->       Just r                
       EnterI1 r ->      Just r               
       EnterI2 r ->      Just r               
       EnterI3 r ->      Just r               
       EnterI4 r ->      Just r               
       EnterI5 r ->      Just r               
       EnterI6 r ->      Just r               
       EnterANeg r ->    Just r             
       EnterXNeg r ->    Just r             
       EnterI1Neg r ->   Just r            
       EnterI2Neg r ->   Just r            
       EnterI3Neg r ->   Just r            
       EnterI4Neg r ->   Just r            
       EnterI5Neg r ->   Just r            
       EnterI6Neg r ->   Just r            
       IncrementA r ->   Just r            
       IncrementX r ->   Just r            
       IncrementI1 r ->  Just r           
       IncrementI2 r ->  Just r          
       IncrementI3 r ->  Just r         
       IncrementI4 r ->  Just r           
       IncrementI5 r ->  Just r           
       IncrementI6 r ->  Just r           
       DecrementA r ->   Just r            
       DecrementX r ->   Just r            
       DecrementI1 r ->  Just r           
       DecrementI2 r ->  Just r           
       DecrementI3 r ->  Just r           
       DecrementI4 r ->  Just r           
       DecrementI5 r ->  Just r           
       DecrementI6 r ->  Just r           
       CompareA r m ->   Just r             
       CompareX r m ->   Just r             
       CompareI1 r m ->  Just r            
       CompareI2 r m ->  Just r            
       CompareI3 r m ->  Just r            
       CompareI4 r m ->  Just r            
       CompareI5 r m ->  Just r            
       CompareI6 r m ->  Just r            
       Jump r ->         Just r                  
       JumpSaveJ r ->    Just r             
       JumpOnOverflow r ->      Just r        
       JumpOnNoOverflow r ->    Just r      
       JumpOnLess r ->          Just r            
       JumpOnEqual r ->         Just r           
       JumpOnGreater r ->       Just r         
       JumpOnGreaterEqual r ->  Just r    
       JumpOnUnEqual r ->       Just r         
       JumpOnLessEqual r ->     Just r       
       JumpANegative r ->       Just r         
       JumpAZero r ->           Just r             
       JumpAPositive r ->       Just r         
       JumpANonNegative r ->    Just r      
       JumpANonZero r ->        Just r          
       JumpANonPositive r ->    Just r      
       JumpXNegative r ->       Just r         
       JumpXZero r ->           Just r             
       JumpXPositive r ->       Just r         
       JumpXNonNegative r ->    Just r      
       JumpXNonZero r ->        Just r          
       JumpXNonPositive r ->    Just r      
       JumpI1Negative r ->      Just r        
       JumpI1Zero r ->          Just r            
       JumpI1Positive r ->      Just r        
       JumpI1NonNegative r ->   Just r     
       JumpI1NonZero r ->       Just r         
       JumpI1NonPositive r ->   Just r     
       JumpI2Negative r ->      Just r        
       JumpI2Zero r ->          Just r            
       JumpI2Positive r ->      Just r        
       JumpI2NonNegative r ->   Just r     
       JumpI2NonZero r ->       Just r         
       JumpI2NonPositive r ->   Just r     
       JumpI3Negative r ->      Just r        
       JumpI3Zero r ->          Just r            
       JumpI3Positive r ->      Just r        
       JumpI3NonNegative r ->   Just r     
       JumpI3NonZero r ->       Just r         
       JumpI3NonPositive r ->   Just r     
       JumpI4Negative r ->      Just r        
       JumpI4Zero r ->          Just r            
       JumpI4Positive r ->      Just r        
       JumpI4NonNegative r ->   Just r     
       JumpI4NonZero r ->       Just r         
       JumpI4NonPositive r ->   Just r     
       JumpI5Negative r ->      Just r        
       JumpI5Zero r ->          Just r            
       JumpI5Positive r ->      Just r        
       JumpI5NonNegative r ->   Just r     
       JumpI5NonZero r ->       Just r         
       JumpI5NonPositive r ->   Just r     
       JumpI6Negative r ->      Just r        
       JumpI6Zero r ->          Just r            
       JumpI6Positive r ->      Just r        
       JumpI6NonNegative r ->   Just r     
       JumpI6NonZero r ->       Just r         
       JumpI6NonPositive r ->   Just r     
       ShiftA r ->              Just r                
       ShiftX r ->              Just r                
       ShiftACircular r ->      Just r        
       ShiftXCircular r ->      Just r        
       SwapAX ->             Nothing
       MoveXI1 ->            Nothing
       MoveXI2 ->            Nothing
       MoveXI3 ->            Nothing
       MoveXI4 ->            Nothing
       MoveXI5 ->            Nothing
       MoveXI6 ->            Nothing
       NoOperation ->        Nothing
       Halt        ->        Nothing  
-}


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
