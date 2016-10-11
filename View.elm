module View exposing (create_row_view, create_field_view, create_controls)


import Html exposing (div, span, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Model exposing (Model)
import Playground exposing (Row, Field, Token(..))
import Update exposing (Msg(..))


create_row_view : Model -> Int -> Row -> Html.Html Msg
create_row_view model y row =
    let
        field_with_coord : Int -> Field -> (Int, Int, Field)
        field_with_coord x field = (x, y, field)
        fields =
            List.map
                (create_field_view model)
                (List.indexedMap field_with_coord row)
    in
        div [class "row"] fields


create_field_view : Model -> (Int, Int, Field) -> Html.Html Msg
create_field_view model (x, y, field) =
    let
        attrs =
            case model.win_streak of
                Nothing ->
                    case field.token of
                        Nothing -> [onClick <| MakeTurn x y, class "field"]
                        Just token -> [class "field"]
                Just line ->
                    if line x y then [class "field win"] else [class "field"]
        content =
            case field.token of
                Nothing -> []
                Just token ->
                    case token of
                        X -> [text "X"]
                        O -> [text "O"]
    in
        div attrs content

create_controls : Html.Html Msg
create_controls =
    div [ class "controls" ]
        [ button[onClick SwapPlayers][text "change_side"]
        ]
