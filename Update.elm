module Update exposing (Msg(..), PlayerType(..),
    make_turn, swap_players, make_AI_turn
    )

import Array
import Random
import Tuple exposing (first, second)

import AI exposing (choices)
import Utils exposing (map_nested_item)
import Model exposing (Model, make_empty_model)
import Playground exposing (Token(..), Field, FieldLine,
    clear_playground, opposite_token, is_center_empty, check_winner
    )


type Msg = MakeTurn Int Int
    | SwapPlayers
    | Choose Int (List (Int, Int))


type PlayerType = Human | Computer


make_turn : Int -> Int -> PlayerType -> Model -> (Model, Cmd Msg)
make_turn x y player model =
    let
        token =
            case player of
                Human -> model.player_token
                Computer -> opposite_token model.player_token
        updated_model = mark_field x y token model
        cmd =
            case updated_model.win_streak of
                Nothing ->
                    case player of
                        Human -> make_decision
                            (opposite_token token)
                            updated_model
                        Computer -> Cmd.none
                Just streak -> Cmd.none
    in
        (updated_model, cmd)


swap_players : Model -> (Model, Cmd Msg)
swap_players model =
    case model.player_token of
        X ->
            let
                new_model =
                    { make_empty_model
                    | player_token = O
                    , playground = clear_playground
                    }
            in
                (new_model, make_decision X new_model)
        O -> (
                { make_empty_model
                | player_token = X
                , playground = clear_playground
                }
                , Cmd.none
             )


make_AI_turn : Int -> List (Int, Int) -> Model -> (Model, Cmd Msg)
make_AI_turn chosen choices model =
    let
        chosen_item = Array.get chosen <| Array.fromList choices
        (x, y) =
            case chosen_item of
                Nothing -> (-1, -1)
                Just item -> item
    in
        make_turn x y Computer model


mark_field : Int -> Int -> Token -> Model -> Model
mark_field x y token model =
    let
        mark : Field -> Field
        mark field = {field | token = Just token}
        updated_playground = map_nested_item model.playground x y mark
    in
        { model
        | playground = updated_playground
        , win_streak = check_winner updated_playground}


make_decision : Token -> Model -> Cmd Msg
make_decision token model =
    let
      make_gen list =
        if List.length list > 0 then Random.int 0 <| (List.length list) - 1
        else Random.int -1 -1
    in
      Random.generate
        (flip(Choose) <| choices token model)
        (make_gen <| choices token model)
