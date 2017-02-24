module TestMixStates exposing (..)

import Mix exposing (..)
import Atom exposing (..)
import Dict


testMult =
    """0: LDA 100,0
     1: JAZ 8,0
     2: ADD 99,0
     3: STA 99,0
     4: LDA 100,0
     5: DECA -1,0
     6: STA 100,0
     7: JMP 0,0
     8: LDA 99,9
     99: 0
     100: 5
     101: 6"""
