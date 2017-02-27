module ExamplePrograms exposing (..)

multiplication =
    """
     100: 0
     101: 5
     102: 6

     0: LDA 101
     1: JAZ 8
     2: ADD 100
     3: STA 100
     4: LDA 102
     5: DECA 1
     6: STA 102
     7: JMP 0
     8: HLT 0"""
