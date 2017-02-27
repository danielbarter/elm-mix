module LineFunctor exposing ( DataFunctor(..)
                            , LineFunctor(..)
                            )

type DataFunctor b = DataNumber Int
                   | DataInstruction b

type LineFunctor a b = LineFunctor a (DataFunctor b)

