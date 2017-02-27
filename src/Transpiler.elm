{-

this code transpiles REL to ASM.
To do this, we build a symbol table to resolve names.

-}

module Transpiler exposing (..)

import CodeFunctor exposing (..)

type TranspileError = NonDeclaredSymbol String
