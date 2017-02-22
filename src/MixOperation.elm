module MixOperation exposing ( MixOperation
                             , RuntimeError
                             )

import Instructions exposing ( CompileTimeError(..)
                             , Address
                             , Index
                             )

type RuntimeError = NoMemoryValue Address
                  | CompileError CompileTimeError
                  | InvalidIndex Index


type alias MixOperation s a = s -> Result RuntimeError (s,a)


-- monad operations

(>>=) : MixOperation s a -> (a -> MixOperation s b) -> MixOperation s b
(>>=) p f = let q s = case p s of
                          Err err -> Err err
                          Ok (ss,x) -> f x ss
            in q

return : a -> MixOperation s a
return x = let p s = Ok (s,x)
           in p

get : MixOperation s s
get = let p s = Ok (s,s)
      in p

put : s -> MixOperation s ()
put m = let p s = Ok (m,())
        in p

throwError : RuntimeError -> MixOperation s a
throwError err = let p s = Err err
                 in p

(<*>) : MixOperation s (a -> b) -> MixOperation s a -> MixOperation s b
(<*>) p q = let r s = case p s of
                          Err err -> Err err
                          Ok (ss,f) -> case q ss of
                                           Err err -> Err err
                                           Ok (sss,x) -> Ok (sss, f x)
            in r

                
(<$>) : ( a -> b ) -> MixOperation s a -> MixOperation s b
(<$>) f p = (return f) <*> p


map2 : ( a -> b -> c) -> MixOperation s a -> MixOperation s b -> MixOperation s c
map2 f p q = ( f <$> p ) <*> q


(<*) : MixOperation s a -> MixOperation s b -> MixOperation s a
(<*) p q = let f x y = x
           in map2 f p q

(*>) : MixOperation s a -> MixOperation s b -> MixOperation s b
(*>) p q = let g x y = y
           in map2 g p q

