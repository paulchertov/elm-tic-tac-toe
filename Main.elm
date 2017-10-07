import Html exposing (div, program)
import Html.Attributes exposing (class)

import Style exposing (style_tag)
import View exposing (create_row_view, create_field_view, create_controls)
import Model exposing (Model, make_empty_model)
import Update exposing (Msg(..), PlayerType(..),
    make_turn, swap_players, make_AI_turn
    )

main =
  program
    { view = view
    , update = update
    , init = init
    , subscriptions = subscriptions
    }


--MODEL--
model : Model
model =
    make_empty_model


--UPDATE--
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MakeTurn x y ->
        make_turn x y Human model
    SwapPlayers ->
        swap_players model
    Choose chosen choices ->
        make_AI_turn chosen choices model


--VIEW--
view : Model -> Html.Html Msg
view model =
    div
        [ class "test-app" ]
        [style_tag,
         div
            [class "playground"]
            (List.indexedMap (create_row_view model) model.playground)
        ,
          create_controls
        ]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


--INIT--
init : (Model, Cmd Msg)
init = (make_empty_model, Cmd.none)
