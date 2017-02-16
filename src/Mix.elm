module Mix exposing (..)

import Dict

import Atom exposing ( Sign(..)
                     , baseExpand
                     , baseContract
                     , Byte
                     , byte
                     , value
                     , mixBase
                     )


type alias SmallRegister = ( Sign, Byte, Byte )
    
type alias BigRegister = ( Sign
                         , Byte
                         , Byte
                         , Byte
                         , Byte
                         , Byte
                         )

type alias Address = Int
type alias Index = Int
type alias Modification = Int
type alias InstructionCode = Int


unpackBig : BigRegister -> ( Address
                           , Index
                           , Modification
                           , InstructionCode
                           )

unpackBig (s,b1,b2,b3,b4,b5) =
    ( baseContract mixBase (s, List.map value [b1,b2])
    , value b3
    , value b4
    , value b5
    )

readSmall : SmallRegister -> Int
readSmall (s,b1,b2) = baseContract mixBase (s,List.map value [b1,b2])

type alias LeftMask = Int
type alias RightMask = Int

getMasks : Modification -> Result RuntimeError (LeftMask,RightMask)
getMasks m =
    case baseExpand 8 m of
        (Pos,[r]) -> if r <= 5
                     then Ok (0,5)
                     else Err <| InvalidModification m
        (Pos,[l,r]) -> if (0 <= l) && (l <= r) && (r <= 5)
                       then Ok (l,r)
                       else Err <| InvalidModification m
        _ -> Err <| InvalidModification m


mask : LeftMask -> RightMask -> BigRegister -> Result RuntimeError BigRegister
mask l r (s,b1,b2,b3,b4,b5) =
    let zero = byte 0
    in case (l,r) of
           (0,0) -> Ok (s,zero,zero,zero,zero,zero)
           (0,1) -> Ok (s,zero,zero,zero,zero,b1)
           (0,2) -> Ok (s,zero,zero,zero,b1,b2)
           (0,3) -> Ok (s,zero,zero,b1,b2,b3)
           (0,4) -> Ok (s,zero,b1,b2,b3,b4)
           (0,5) -> Ok (s,b1,b2,b3,b4,b5)
           (1,1) -> Ok (Pos,zero,zero,zero,zero,b1)
           (1,2) -> Ok (Pos,zero,zero,zero,b1,b2)
           (1,3) -> Ok (Pos,zero,zero,b1,b2,b3)
           (1,4) -> Ok (Pos,zero,b1,b2,b3,b4)
           (1,5) -> Ok (Pos,b1,b2,b3,b4,b5)
           (2,2) -> Ok (Pos,zero,zero,zero,zero,b2)
           (2,3) -> Ok (Pos,zero,zero,zero,b2,b3)
           (2,4) -> Ok (Pos,zero,zero,b2,b3,b4)
           (2,5) -> Ok (Pos,zero,b2,b3,b4,b5)
           (3,3) -> Ok (Pos,zero,zero,zero,zero,b3)
           (3,4) -> Ok (Pos,zero,zero,zero,b3,b4)
           (3,5) -> Ok (Pos,zero,zero,b3,b4,b5)
           (4,4) -> Ok (Pos,zero,zero,zero,zero,b4)
           (4,5) -> Ok (Pos,zero,zero,zero,b4,b5)
           (5,5) -> Ok (Pos,zero,zero,zero,zero,b5)
           _ -> Err <| InvalidMask l r
                       
type alias Memory = Dict.Dict Address BigRegister

type OverflowToggle = Overflow | Good
type ComparisonIndicator = L | E | G

type alias Mix = { a   : BigRegister
                 , x   : BigRegister
                 , i1  : SmallRegister
                 , i2  : SmallRegister
                 , i3  : SmallRegister
                 , i4  : SmallRegister
                 , i5  : SmallRegister
                 , i6  : SmallRegister
                 , j   : SmallRegister
                 , p   : Address -- program counter
                 , mem : Memory
                 , overflow : OverflowToggle
                 , comparison : ComparisonIndicator
                 }

testLoadA : Mix
testLoadA = let br = (Pos,byte 0,byte 0,byte 0,byte 0,byte 0)
                sr = (Pos, byte 0, byte 0)
                m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 0,byte 29,byte 8))
                                  , (2000,(Neg,byte 0,byte 80,byte 3,byte 5,byte 4))
                                  ]
            in { a = br
               , x = br
               , i1 = sr
               , i2 = sr
               , i3 = sr
               , i4 = sr
               , i5 = sr
               , i6 = sr
               , j = sr
               , p = 0
               , mem = m
               , overflow = Good
               , comparison = E
               }

{-

execution cycle:
  get instruction; increment program counter
  decode instruction
  execute instruction
-}

step : Mix -> Result RuntimeError Mix
step s = case getInstruction s of
             Err err -> Err err
             Ok (ss,r) -> case decodeInstruction r of
                              Err err -> Err err
                              Ok i -> executeInstruction i ss

getInstruction : Mix -> Result RuntimeError (Mix,BigRegister)
getInstruction s = case Dict.get s.p s.mem of
                       Nothing -> Err <| NoMemoryValue s.p
                       Just r  -> Ok ( { s | p = s.p + 1 }, r)

decodeInstruction : BigRegister -> Result RuntimeError Instruction
decodeInstruction r =
    case unpackBig r of
        (a,i,m,8) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadA a i l r
        (_,_,_,c) -> Err <| UnrecognizedInstructionCode c
        

executeInstruction : Instruction -> Mix -> Result RuntimeError Mix
executeInstruction inst s =
    case inst of
        LoadA a i l r -> case Result.map readSmall <| getIndexRegister i s of
                             Err err -> Err err
                             Ok v -> case Dict.get (a+v) s.mem of
                                         Nothing -> Err <| NoMemoryValue (a+v)
                                         Just aa -> case mask l r aa of
                                                        Err err -> Err err
                                                        Ok newA -> Ok { s
                                                                      | a = newA
                                                                      }


getIndexRegister : Index -> Mix -> Result RuntimeError SmallRegister
getIndexRegister i m =
    case i of
        0 -> Ok (Pos,byte 0,byte 0)
        1 -> Ok m.i1
        2 -> Ok m.i2
        3 -> Ok m.i3
        4 -> Ok m.i4
        5 -> Ok m.i5
        6 -> Ok m.i6
        _ -> Err <| InvalidIndex i
    


type RuntimeError = NoMemoryValue Address
                  | InvalidModification Modification
                  | UnrecognizedInstructionCode InstructionCode
                  | InvalidIndex Index
                  | InvalidMask LeftMask RightMask

type Instruction = LoadA Address Index LeftMask RightMask
