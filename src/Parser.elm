module Parser exposing (..)

import StateMonad exposing (..)
import Instruction exposing (..)
import Atom exposing (..)
import Tokenizer exposing (..)



type alias InternalRep = ( Maybe RelativeAddress
                         , Maybe Int
                         , Tag
                         , Maybe RelativeAddress
                         , Maybe Index
                         )

type ParseError = UnexpectedToken
                | EndOfStream
type alias Parser a = State (List Token) ParseError a


getToken : Parser Token
getToken =
    let getToken s =
            case s of
                []      -> throwError EndOfStream
                (t::ts) -> (put ts) *> return t
    in get >>= getToken

handleRelativeAddress : Token -> Parser (Maybe RelativeAddress)
handleRelativeAddress t =
    case t of
        Lab name -> getToken *> (return <| Just <| Label name)
        N n      -> getToken *> (return <| Just <| Value n)
        _ -> ( ( (\l -> t :: l) <$> get) >>= put ) *> return Nothing

handleInt : Token -> Parser (Maybe Int)
handleInt t =
    case t of
        N n -> (getToken *> (return <| Just n))
        _ -> ( ( (\l -> t :: l) <$> get) >>= put ) *> return Nothing

handleTag : Token -> Parser (Tag)
handleTag t =
    case t of
        I inst -> return inst
        _ -> throwError UnexpectedToken


parseInstruction : Parser InternalRep
parseInstruction = map5 (,,,,)
            (try (getToken >>= handleRelativeAddress) (return Nothing))
            (try (getToken >>= handleInt) (return Nothing))
            (getToken >>= handleTag)
            (try (getToken >>= handleRelativeAddress) (return Nothing))
            (try (getToken >>= handleInt) (return Nothing))
