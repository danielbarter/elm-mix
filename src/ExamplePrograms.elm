{-

example programs

-}

module ExamplePrograms exposing (..)

multiplication =
    """
     s: 0
     a: 5
     b: 6

     LDA 101
     JAZ 8
     ADD 100
     STA 100
     LDA 102
     DECA 1
     STA 102
     s:JMP 0
     HLT 0"""
