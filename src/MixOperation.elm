module MixOperation exposing ( MixOperation
                             , State
                             , RuntimeError(..)
                             , (>>=)
                             , return
                             , get
                             , put
                             , throwError
                             , (<*>)
                             , (<$>)
                             , map2
                             , (<*)
                             , (*>)
                             )

import Instructions exposing (..)

type RuntimeError = NoMemoryValue Address
                  | CompileError CompileTimeError
                  | InvalidIndex Index


type alias MixOperation s a = State s RuntimeError a


-- State Monad

type alias State s e a = s -> Result e (s,a)

(>>=) : State s e a -> (a -> State s e b) -> State s e b
(>>=) p f = let q s = case p s of
                          Err err -> Err err
                          Ok (ss,x) -> f x ss
            in q

return : a -> State s e a
return x = let p s = Ok (s,x)
           in p

get : State s e s
get = let p s = Ok (s,s)
      in p

put : s -> State s e ()
put m = let p s = Ok (m,())
        in p

throwError : e -> State s e a
throwError err = let p s = Err err
                 in p

(<*>) : State s e (a -> b) -> State s e a -> State s e b
(<*>) p q = let r s = case p s of
                          Err err -> Err err
                          Ok (ss,f) -> case q ss of
                                           Err err -> Err err
                                           Ok (sss,x) -> Ok (sss, f x)
            in r

                
(<$>) : ( a -> b ) -> State s e a -> State s e b
(<$>) f p = (return f) <*> p


map2 : ( a -> b -> c) -> State s e a -> State s e b -> State s e c
map2 f p q = ( f <$> p ) <*> q


(<*) : State s e a -> State s e b -> State s e a
(<*) p q = let f x y = x
           in map2 f p q

(*>) : State s e a -> State s e b -> State s e b
(*>) p q = let g x y = y
           in map2 g p q

