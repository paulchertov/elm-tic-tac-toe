module Model exposing(Model, make_empty_model)


import Playground exposing (Playground, Token(..), FieldLine, clear_playground)


type alias Model =
    { playground: Playground
    , player_token: Token
    , win_streak: Maybe FieldLine
    }


make_empty_model : Model
make_empty_model =
    { playground = clear_playground
    , player_token = X
    , win_streak = Nothing
    }
