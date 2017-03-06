{-

example programs

(:label) (/mask) instruction (relative address) (+index)

-}

module ExamplePrograms exposing (..)

ex1 =
    """
     :start LDA y
     JAZ end
     DECA 1
     STA y
     LDA x
     ADD s
     STA s
     JMP start
     :end LDA s
     HLT

     # data
     :s 0
     :x 5
     :y 6           
     """
