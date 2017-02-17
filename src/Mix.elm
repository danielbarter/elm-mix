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

flipSignBig : BigRegister -> BigRegister
flipSignBig (s,b1,b2,b3,b4,b5) = case s of
                                     Pos -> (Neg,b1,b2,b3,b4,b5)
                                     Neg -> (Pos,b1,b2,b3,b4,b5)

flipSignSmall : SmallRegister -> SmallRegister
flipSignSmall (s,b1,b2) = case s of
                              Pos -> (Neg,b1,b2)
                              Neg -> (Pos,b1,b2)
    
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

maskSmall : LeftMask
          -> RightMask
          -> BigRegister
          -> Result RuntimeError SmallRegister
maskSmall l r (s,b1,b2,b3,b4,b5) =
    let zero = byte 0
    in case (l,r) of
           (0,0) -> Ok (s,zero,zero)
           (0,1) -> Ok (s,zero,b1)
           (0,2) -> Ok (s,b1,b2)
           (0,3) -> Ok (s,b2,b3)
           (0,4) -> Ok (s,b3,b4)
           (0,5) -> Ok (s,b4,b5)
           (1,1) -> Ok (Pos,zero,b1)
           (1,2) -> Ok (Pos,b1,b2)
           (1,3) -> Ok (Pos,b2,b3)
           (1,4) -> Ok (Pos,b3,b4)
           (1,5) -> Ok (Pos,b4,b5)
           (2,2) -> Ok (Pos,zero,b2)
           (2,3) -> Ok (Pos,b2,b3)
           (2,4) -> Ok (Pos,b3,b4)
           (2,5) -> Ok (Pos,b4,b5)
           (3,3) -> Ok (Pos,zero,b3)
           (3,4) -> Ok (Pos,b3,b4)
           (3,5) -> Ok (Pos,b4,b5)
           (4,4) -> Ok (Pos,zero,b4)
           (4,5) -> Ok (Pos,b4,b5)
           (5,5) -> Ok (Pos,zero,b5)
           _ -> Err <| InvalidMask l r


storeBig : LeftMask
         -> RightMask
         -> BigRegister
         -> BigRegister -> Result RuntimeError BigRegister

storeBig l r (msn,m1,m2,m3,m4,m5) (asn,a1,a2,a3,a4,a5)
    = case (l,r) of
          (0,0) -> Ok (asn,m1,m2,m3,m4,m5)
          (0,1) -> Ok (asn,a5,m2,m3,m4,m5)
          (0,2) -> Ok (asn,a4,a5,m3,m4,m5)
          (0,3) -> Ok (asn,a3,a4,a5,m4,m5)
          (0,4) -> Ok (asn,a2,a3,a4,a5,m5)
          (0,5) -> Ok (asn,a1,a2,a3,a4,a5)
          (1,1) -> Ok (msn,a5,m2,m3,m4,m5)
          (1,2) -> Ok (msn,a4,a5,m3,m4,m5)
          (1,3) -> Ok (msn,a3,a4,a5,m4,m5)
          (1,4) -> Ok (msn,a2,a3,a4,a5,m5)
          (1,5) -> Ok (msn,a1,a2,a3,a4,a5)
          (2,2) -> Ok (msn,m1,a5,m3,m4,m5)
          (2,3) -> Ok (msn,m1,a4,a5,m4,m5)
          (2,4) -> Ok (msn,m1,a3,a4,a5,m5)
          (2,5) -> Ok (msn,m1,a2,a3,a4,a5)
          (3,3) -> Ok (msn,m1,m2,a5,m4,m5)
          (3,4) -> Ok (msn,m1,m2,a4,a5,m5)
          (3,5) -> Ok (msn,m1,m2,a3,a4,a5)
          (4,4) -> Ok (msn,m1,m2,m3,a5,m5)
          (4,5) -> Ok (msn,m1,m2,m3,a4,a5)
          (5,5) -> Ok (msn,m1,m2,m3,m4,a5)
          _ -> Err <| InvalidMask l r

storeSmall : LeftMask
           -> RightMask
           -> BigRegister
           -> SmallRegister
           -> Result RuntimeError BigRegister
storeSmall l r b (s,i1,i2) = storeBig l r b (s,byte 0,byte 0,byte 0,i1,i2)
                
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

{-

execution cycle:
  get instruction; increment program counter
  decode instruction
  execute instruction
-}


type RuntimeError = NoMemoryValue Address
                  | InvalidModification Modification
                  | UnrecognizedInstructionCode InstructionCode
                  | InvalidIndex Index
                  | InvalidMask LeftMask RightMask

{-

when adding an instruction, you need to update
  decodeInstruction
  executeInstruction
-}

type Instruction = LoadA Address Index LeftMask RightMask
                 | LoadX Address Index LeftMask RightMask
                 | LoadI1 Address Index LeftMask RightMask
                 | LoadI2 Address Index LeftMask RightMask
                 | LoadI3 Address Index LeftMask RightMask
                 | LoadI4 Address Index LeftMask RightMask
                 | LoadI5 Address Index LeftMask RightMask
                 | LoadI6 Address Index LeftMask RightMask
                 | LoadANeg Address Index LeftMask RightMask
                 | LoadXNeg Address Index LeftMask RightMask
                 | LoadI1Neg Address Index LeftMask RightMask
                 | LoadI2Neg Address Index LeftMask RightMask
                 | LoadI3Neg Address Index LeftMask RightMask
                 | LoadI4Neg Address Index LeftMask RightMask
                 | LoadI5Neg Address Index LeftMask RightMask
                 | LoadI6Neg Address Index LeftMask RightMask
                 | StoreA Address Index LeftMask RightMask
                 | StoreX Address Index LeftMask RightMask
                 | StoreI1 Address Index LeftMask RightMask
                 | StoreI2 Address Index LeftMask RightMask
                 | StoreI3 Address Index LeftMask RightMask
                 | StoreI4 Address Index LeftMask RightMask
                 | StoreI5 Address Index LeftMask RightMask
                 | StoreI6 Address Index LeftMask RightMask

                   
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
        (a,i,m,15) -> case getMasks m of
                          Err err  -> Err err
                          Ok (l,r) -> Ok <| LoadX a i l r
        (a,i,m,9) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI1 a i l r
        (a,i,m,10) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI2 a i l r 
        (a,i,m,11) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI3 a i l r
        (a,i,m,12) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI4 a i l r
        (a,i,m,13) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI5 a i l r
        (a,i,m,14) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI6 a i l r
        (a,i,m,16) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadANeg a i l r
        (a,i,m,23) -> case getMasks m of
                          Err err  -> Err err
                          Ok (l,r) -> Ok <| LoadXNeg a i l r
        (a,i,m,17) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI1Neg a i l r
        (a,i,m,18) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI2Neg a i l r 
        (a,i,m,19) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI3Neg a i l r
        (a,i,m,20) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI4Neg a i l r
        (a,i,m,21) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI5Neg a i l r
        (a,i,m,22) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| LoadI6Neg a i l r
        (a,i,m,24) -> case getMasks m of
                          Err err  -> Err err
                          Ok (l,r) -> Ok <| StoreA a i l r
        (a,i,m,31) -> case getMasks m of
                          Err err  -> Err err
                          Ok (l,r) -> Ok <| StoreX a i l r
        (a,i,m,25) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| StoreI1 a i l r
        (a,i,m,26) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| StoreI2 a i l r 
        (a,i,m,27) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| StoreI3 a i l r
        (a,i,m,28) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| StoreI4 a i l r
        (a,i,m,29) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| StoreI5 a i l r
        (a,i,m,30) -> case getMasks m of
                         Err err  -> Err err
                         Ok (l,r) -> Ok <| StoreI6 a i l r
        (_,_,_,c) -> Err <| UnrecognizedInstructionCode c
        

executeInstruction : Instruction -> Mix -> Result RuntimeError Mix
executeInstruction inst s =
    case inst of
        LoadA a i l r -> case Result.map readSmall <| getIndexRegister i s of
                             Err err -> Err err
                             Ok v -> case Dict.get (a+v) s.mem of
                                         Nothing -> Err <| NoMemoryValue (a+v)
                                         Just br -> case mask l r br of
                                                        Err err -> Err err
                                                        Ok newA -> Ok { s
                                                                      | a = newA
                                                                      }
        LoadX a i l r -> case Result.map readSmall <| getIndexRegister i s of
                             Err err -> Err err
                             Ok v -> case Dict.get (a+v) s.mem of
                                         Nothing -> Err <| NoMemoryValue (a+v)
                                         Just br  -> case mask l r br of
                                                        Err err -> Err err
                                                        Ok newX -> Ok { s
                                                                      | x = newX
                                                                      }
        LoadI1 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                              Err err -> Err err
                              Ok v -> case Dict.get (a+v) s.mem of
                                          Nothing -> Err <| NoMemoryValue (a+v)
                                          Just br  -> case maskSmall l r br of
                                                         Err err -> Err err
                                                         Ok newI1 -> Ok { s
                                                                        | i1 = newI1
                                                                        }
        LoadI2 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                              Err err -> Err err
                              Ok v -> case Dict.get (a+v) s.mem of
                                          Nothing -> Err <| NoMemoryValue (a+v)
                                          Just br  -> case maskSmall l r br of
                                                         Err err -> Err err
                                                         Ok newI2 -> Ok { s
                                                                        | i2 = newI2
                                                                        }
        LoadI3 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                              Err err -> Err err
                              Ok v -> case Dict.get (a+v) s.mem of
                                          Nothing -> Err <| NoMemoryValue (a+v)
                                          Just br  -> case maskSmall l r br of
                                                         Err err -> Err err
                                                         Ok newI3 -> Ok { s
                                                                        | i3 = newI3
                                                                        }
        LoadI4 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                              Err err -> Err err
                              Ok v -> case Dict.get (a+v) s.mem of
                                          Nothing -> Err <| NoMemoryValue (a+v)
                                          Just br  -> case maskSmall l r br of
                                                         Err err -> Err err
                                                         Ok newI4 -> Ok { s
                                                                        | i4 = newI4
                                                                        }
        LoadI5 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                              Err err -> Err err
                              Ok v -> case Dict.get (a+v) s.mem of
                                          Nothing -> Err <| NoMemoryValue (a+v)
                                          Just br  -> case maskSmall l r br of
                                                         Err err -> Err err
                                                         Ok newI5 -> Ok { s
                                                                        | i5 = newI5
                                                                        }
        LoadI6 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                              Err err -> Err err
                              Ok v -> case Dict.get (a+v) s.mem of
                                          Nothing -> Err <| NoMemoryValue (a+v)
                                          Just br  -> case maskSmall l r br of
                                                         Err err -> Err err
                                                         Ok newI6 -> Ok { s
                                                                        | i6 = newI6
                                                                        }

        LoadANeg a i l r -> case Result.map readSmall <| getIndexRegister i s of
                                Err err -> Err err
                                Ok v -> case Dict.get (a+v) s.mem of
                                            Nothing -> Err <| NoMemoryValue (a+v)
                                            Just br -> case mask l r br of
                                                           Err err -> Err err
                                                           Ok newA -> Ok { s
                                                                         | a = flipSignBig newA
                                                                         }
        LoadXNeg a i l r -> case Result.map readSmall <| getIndexRegister i s of
                                Err err -> Err err
                                Ok v -> case Dict.get (a+v) s.mem of
                                            Nothing -> Err <| NoMemoryValue (a+v)
                                            Just br  -> case mask l r br of
                                                           Err err -> Err err
                                                           Ok newX -> Ok { s
                                                                         | x = flipSignBig newX
                                                                         }
        LoadI1Neg a i l r -> case Result.map readSmall <| getIndexRegister i s of
                                 Err err -> Err err
                                 Ok v -> case Dict.get (a+v) s.mem of
                                             Nothing -> Err <| NoMemoryValue (a+v)
                                             Just br  -> case maskSmall l r br of
                                                            Err err -> Err err
                                                            Ok newI1 -> Ok { s
                                                                           | i1 = flipSignSmall newI1
                                                                           }
        LoadI2Neg a i l r -> case Result.map readSmall <| getIndexRegister i s of
                                 Err err -> Err err
                                 Ok v -> case Dict.get (a+v) s.mem of
                                             Nothing -> Err <| NoMemoryValue (a+v)
                                             Just br  -> case maskSmall l r br of
                                                            Err err -> Err err
                                                            Ok newI2 -> Ok { s
                                                                           | i2 = flipSignSmall newI2
                                                                           }
        LoadI3Neg a i l r -> case Result.map readSmall <| getIndexRegister i s of
                                 Err err -> Err err
                                 Ok v -> case Dict.get (a+v) s.mem of
                                             Nothing -> Err <| NoMemoryValue (a+v)
                                             Just br  -> case maskSmall l r br of
                                                            Err err -> Err err
                                                            Ok newI3 -> Ok { s
                                                                           | i3 = flipSignSmall newI3
                                                                           }
        LoadI4Neg a i l r -> case Result.map readSmall <| getIndexRegister i s of
                                 Err err -> Err err
                                 Ok v -> case Dict.get (a+v) s.mem of
                                             Nothing -> Err <| NoMemoryValue (a+v)
                                             Just br  -> case maskSmall l r br of
                                                            Err err -> Err err
                                                            Ok newI4 -> Ok { s
                                                                           | i4 = flipSignSmall newI4
                                                                           }
        LoadI5Neg a i l r -> case Result.map readSmall <| getIndexRegister i s of
                                 Err err -> Err err
                                 Ok v -> case Dict.get (a+v) s.mem of
                                             Nothing -> Err <| NoMemoryValue (a+v)
                                             Just br  -> case maskSmall l r br of
                                                            Err err -> Err err
                                                            Ok newI5 -> Ok { s
                                                                           | i5 = flipSignSmall newI5
                                                                           }
        LoadI6Neg a i l r -> case Result.map readSmall <| getIndexRegister i s of
                                 Err err -> Err err
                                 Ok v -> case Dict.get (a+v) s.mem of
                                             Nothing -> Err <| NoMemoryValue (a+v)
                                             Just br  -> case maskSmall l r br of
                                                            Err err -> Err err
                                                            Ok newI6 -> Ok { s
                                                                           | i6 = flipSignSmall newI6
                                                                           }
        StoreA a i l r -> case Result.map readSmall <| getIndexRegister i s of
                              Err err -> Err err
                              Ok v -> case Dict.get (a+v) s.mem of
                                          Nothing -> case (storeBig l r (Pos,byte 0,byte 0,byte 0,byte 0,byte 0) s.a) of
                                                         Err err -> Err err
                                                         Ok mm -> Ok { s
                                                                     | mem = Dict.insert (a+v) mm s.mem
                                                                     }
                                          Just m -> case (storeBig l r m s.a) of
                                                        Err err -> Err err
                                                        Ok mm -> Ok { s
                                                                    | mem = Dict.insert (a+v) mm s.mem
                                                                    }
        StoreX a i l r -> case Result.map readSmall <| getIndexRegister i s of
                              Err err -> Err err
                              Ok v -> case Dict.get (a+v) s.mem of
                                          Nothing -> case (storeBig l r (Pos,byte 0,byte 0,byte 0,byte 0,byte 0) s.x) of
                                                         Err err -> Err err
                                                         Ok mm -> Ok { s
                                                                     | mem = Dict.insert (a+v) mm s.mem
                                                                     }
                                          Just m -> case (storeBig l r m s.x) of
                                                        Err err -> Err err
                                                        Ok mm -> Ok { s
                                                                    | mem = Dict.insert (a+v) mm s.mem
                                                                    }
        StoreI1 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                               Err err -> Err err
                               Ok v -> case Dict.get (a+v) s.mem of
                                           Nothing -> case (storeSmall l r (Pos,byte 0,byte 0,byte 0,byte 0,byte 0) s.i1) of
                                                          Err err -> Err err
                                                          Ok mm -> Ok { s
                                                                      | mem = Dict.insert (a+v) mm s.mem
                                                                      }
                                           Just m -> case (storeSmall l r m s.i1) of
                                                         Err err -> Err err
                                                         Ok mm -> Ok { s
                                                                     | mem = Dict.insert (a+v) mm s.mem
                                                                     }
        StoreI2 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                               Err err -> Err err
                               Ok v -> case Dict.get (a+v) s.mem of
                                           Nothing -> case (storeSmall l r (Pos,byte 0,byte 0,byte 0,byte 0,byte 0) s.i2) of
                                                          Err err -> Err err
                                                          Ok mm -> Ok { s
                                                                      | mem = Dict.insert (a+v) mm s.mem
                                                                      }
                                           Just m -> case (storeSmall l r m s.i2) of
                                                         Err err -> Err err
                                                         Ok mm -> Ok { s
                                                                     | mem = Dict.insert (a+v) mm s.mem
                                                                     }
        StoreI3 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                               Err err -> Err err
                               Ok v -> case Dict.get (a+v) s.mem of
                                           Nothing -> case (storeSmall l r (Pos,byte 0,byte 0,byte 0,byte 0,byte 0) s.i3) of
                                                          Err err -> Err err
                                                          Ok mm -> Ok { s
                                                                      | mem = Dict.insert (a+v) mm s.mem
                                                                      }
                                           Just m -> case (storeSmall l r m s.i3) of
                                                         Err err -> Err err
                                                         Ok mm -> Ok { s
                                                                     | mem = Dict.insert (a+v) mm s.mem
                                                                     }
        StoreI4 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                               Err err -> Err err
                               Ok v -> case Dict.get (a+v) s.mem of
                                           Nothing -> case (storeSmall l r (Pos,byte 0,byte 0,byte 0,byte 0,byte 0) s.i4) of
                                                          Err err -> Err err
                                                          Ok mm -> Ok { s
                                                                      | mem = Dict.insert (a+v) mm s.mem
                                                                      }
                                           Just m -> case (storeSmall l r m s.i4) of
                                                         Err err -> Err err
                                                         Ok mm -> Ok { s
                                                                     | mem = Dict.insert (a+v) mm s.mem
                                                                     }
        StoreI5 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                               Err err -> Err err
                               Ok v -> case Dict.get (a+v) s.mem of
                                           Nothing -> case (storeSmall l r (Pos,byte 0,byte 0,byte 0,byte 0,byte 0) s.i5) of
                                                          Err err -> Err err
                                                          Ok mm -> Ok { s
                                                                      | mem = Dict.insert (a+v) mm s.mem
                                                                      }
                                           Just m -> case (storeSmall l r m s.i5) of
                                                         Err err -> Err err
                                                         Ok mm -> Ok { s
                                                                     | mem = Dict.insert (a+v) mm s.mem
                                                                     }
        StoreI6 a i l r -> case Result.map readSmall <| getIndexRegister i s of
                               Err err -> Err err
                               Ok v -> case Dict.get (a+v) s.mem of
                                           Nothing -> case (storeSmall l r (Pos,byte 0,byte 0,byte 0,byte 0,byte 0) s.i6) of
                                                          Err err -> Err err
                                                          Ok mm -> Ok { s
                                                                      | mem = Dict.insert (a+v) mm s.mem
                                                                      }
                                           Just m -> case (storeSmall l r m s.i6) of
                                                         Err err -> Err err
                                                         Ok mm -> Ok { s
                                                                     | mem = Dict.insert (a+v) mm s.mem
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

testLoadX : Mix
testLoadX = let br = (Pos,byte 0,byte 0,byte 0,byte 0,byte 0)
                sr = (Pos, byte 0, byte 0)
                m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 3,byte 29,byte 15))
                                  , (1899,(Neg,byte 0,byte 80,byte 3,byte 5,byte 4))
                                  ]
            in { a = br
               , x = br
               , i1 = sr
               , i2 = sr
               , i3 = (Neg,byte 01,byte 01)
               , i4 = sr
               , i5 = sr
               , i6 = sr
               , j = sr
               , p = 0
               , mem = m
               , overflow = Good
               , comparison = E
               }

testLoadI1 : Mix
testLoadI1 = let br = (Pos,byte 0,byte 0,byte 0,byte 0,byte 0)
                 sr = (Pos, byte 0, byte 0)
                 m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 3,byte 5,byte 9))
                                   , (1899,(Neg,byte 0,byte 80,byte 3,byte 5,byte 4))
                                   ]
             in { a = br
                , x = br
                , i1 = sr
                , i2 = sr
                , i3 = (Neg,byte 01,byte 01)
                , i4 = sr
                , i5 = sr
                , i6 = sr
                , j = sr
                , p = 0
                , mem = m
                , overflow = Good
                , comparison = E
                }

testLoadI6 : Mix
testLoadI6 = let br = (Pos,byte 0,byte 0,byte 0,byte 0,byte 0)
                 sr = (Pos, byte 0, byte 0)
                 m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 3,byte 13,byte 14))
                                   , (1899,(Neg,byte 0,byte 80,byte 3,byte 5,byte 4))
                                   ]
             in { a = br
                , x = br
                , i1 = sr
                , i2 = sr
                , i3 = (Neg,byte 01,byte 01)
                , i4 = sr
                , i5 = sr
                , i6 = sr
                , j = sr
                , p = 0
                , mem = m
                , overflow = Good
                , comparison = E
                }


testStoreA : Mix
testStoreA = let br = (Pos,byte 0,byte 0,byte 0,byte 0,byte 0)
                 sr = (Pos, byte 0, byte 0)
                 m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 3,byte 19,byte 24))
                                   , (1899,(Neg,byte 1,byte 2,byte 3,byte 4,byte 5))
                                   ]
             in { a = (Pos,byte 6,byte 7,byte 8,byte 9,byte 0)
                , x = br
                , i1 = sr
                , i2 = sr
                , i3 = (Neg,byte 01,byte 01)
                , i4 = sr
                , i5 = sr
                , i6 = sr
                , j = sr
                , p = 0
                , mem = m
                , overflow = Good
                , comparison = E
                }

testStoreX : Mix
testStoreX = let br = (Pos,byte 0,byte 0,byte 0,byte 0,byte 0)
                 sr = (Pos, byte 0, byte 0)
                 m = Dict.fromList [ (0,(Pos,byte 20,byte 0,byte 3,byte 20,byte 31))
                                   , (1899,(Neg,byte 1,byte 2,byte 3,byte 4,byte 5))
                                   ]
             in { a = br
                , x = (Pos,byte 6,byte 7,byte 8,byte 9,byte 0)
                , i1 = sr
                , i2 = sr
                , i3 = (Neg,byte 01,byte 01)
                , i4 = sr
                , i5 = sr
                , i6 = sr
                , j = sr
                , p = 0
                , mem = m
                , overflow = Good
                , comparison = E
                }
