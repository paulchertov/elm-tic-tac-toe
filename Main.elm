import Html exposing (nav, div, span, text, button)
import Html.Attributes exposing (class, style, type')
import Html.Events exposing (onClick)
import Html.App exposing (program)
import List exposing (
        length,
        any, all,
        head, tail,
        concat,
        filter, map, map2, indexedMap, filterMap
    )
import Dict
import Array
import Random

main =
  program
    { view = view
    , update = update
    , init = init
    , subscriptions = subscriptions
    }

--MODEL--
type alias Model =
    { playground: Playground
    , player_token: Token
    , win_streak: Maybe FieldLine
    }
model : Model
model =
    make_empty_model

make_empty_model : Model
make_empty_model =
    { playground = clear_playground
    , player_token = X
    , win_streak = Nothing
    }

type alias Playground = List Row
clear_playground : Playground
clear_playground =
    [ clear_row
    , clear_row
    , clear_row
    ]

type alias Row = List Field
clear_row : Row
clear_row =
    [ empty_field
    , empty_field
    , empty_field
    ]

type alias Field =
    { token: Maybe Token }
empty_field : Field
empty_field =
    {token = Nothing}

type alias FieldLine = Int -> Int -> Bool
all_lines =
    concat[
      map i_row [0..2],
      map i_col [0..2],
      [hack_diagonal, slash_diagonal]
    ]
i_row : Int -> FieldLine
i_row i = (\x -> \y -> y == i)
i_col : Int -> FieldLine
i_col i = (\x -> \y -> x == i)
hack_diagonal : FieldLine
hack_diagonal = (\x -> \y -> 2 - x == y)
slash_diagonal : FieldLine
slash_diagonal = (\x -> \y -> y == x)



type Token = X | O
type PlayerType = Human | Computer

--UPDATE--
type Msg = MakeTurn Int Int
    | SwapPlayers
    | Choose Int (List (Int, Int))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MakeTurn x y ->
        make_turn x y Human model
    SwapPlayers ->
        swap_players model
    Choose chosen choices ->
        make_AI_turn chosen choices model

--Updates--

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
        X -> (
                { make_empty_model
                | player_token = O
                , playground = clear_playground
                }
                , Cmd.none
             )
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

--Turn--
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


check_winner : Playground -> Maybe FieldLine
check_winner playground =
    let
        is_winner : FieldLine -> Bool
        is_winner line =
            let
                in_line x y item =
                    if line x y then Just item else Nothing
                diagonal =
                    nested_indexed_map playground in_line
                zeros item = is_field_of_kind O item
                crosses item = is_field_of_kind X item
            in
                all zeros diagonal || all crosses diagonal
    in
        head <| filter is_winner all_lines

--AI--
type LineType =
    Empty
    | OneToWin
    | OneToLose
    | OneProtagonist
    | OneAntagonist
    | TwoOfDifferentKind
    | Stuck
    | AntagonistWin
    | ProtagonistWin

make_decision : Token -> Model -> Cmd Msg
make_decision token model =
    let
        make_filter token =
            (\a -> if snd a == token then Just (fst a) else Nothing)
        playground = model.playground
        possibilities = map_possibilities token playground
        ones_to_win = filterMap (make_filter OneToWin) possibilities
        ones_to_lose = filterMap (make_filter OneToLose) possibilities
        two_to_win = filterMap (make_filter OneProtagonist) possibilities
        two_to_lose = filterMap (make_filter  OneAntagonist) possibilities

        make_gen list =
            if length list > 0 then Random.int 0 <| (length list) - 1
            else Random.int -1 -1
        mapper_with_playground =
            flip coordinates_of_empties_in_line playground
        choices =
            if length ones_to_win > 0
                then List.concat (map mapper_with_playground ones_to_win)
            else if length ones_to_lose > 0
                then List.concat (map mapper_with_playground ones_to_lose)
            else if is_center_empty playground
                then [(1,1)]
            else if length two_to_win > 0
                then List.concat (map mapper_with_playground two_to_win)
            else if length two_to_lose > 0
                then List.concat (map mapper_with_playground two_to_lose)
            else coordinates_of_all_empties playground
    in
        Random.generate
            (flip(Choose) choices)
            (make_gen choices)


map_possibilities : Token -> Playground -> List (FieldLine, LineType)
map_possibilities token playground =
    let
        content_extractor = flip line_content playground
        lines_content =  map content_extractor all_lines
    in
        map2 (,) all_lines <| map (line_type token) lines_content

is_center_empty : Playground -> Bool
is_center_empty playground =
    let
        is_empty_center x y item =
            if (x == 1
                && y ==1
                && case item.token of
                    Nothing -> True
                    Just token -> False
                ) then Just item
            else Nothing
    in
        case head <| nested_indexed_map playground is_empty_center of
            Just item -> True
            Nothing -> False

coordinates_of_empties_in_line : FieldLine -> Playground -> List (Int, Int)
coordinates_of_empties_in_line line playground =
    let
        mapper : Int -> Int -> Field -> Maybe (Int, Int)
        mapper x y item =
            if (line x y
                && case item.token of
                    Nothing -> True
                    Just token -> False
            ) then Just (x, y)
            else Nothing
    in
        nested_indexed_map playground mapper

coordinates_of_all_empties : Playground -> List (Int, Int)
coordinates_of_all_empties playground =
    let
        mapper : Int -> Int -> Field -> Maybe (Int, Int)
        mapper x y item =
            case item.token of
                Nothing -> Just (x,y)
                Just token -> Nothing
    in
        nested_indexed_map playground mapper


line_content : FieldLine -> Playground -> Row
line_content line playground =
    let
        in_line x y item =
            if line x y then Just item else Nothing
    in
        nested_indexed_map playground in_line

line_type : Token -> Row -> LineType
line_type token row =
    let
        protagonist = count_of_kind token row
        antagonist = count_of_kind (opposite_token token) row
    in
        if protagonist == 0
            then if antagonist == 0 then Empty
            else if antagonist == 1 then OneAntagonist
            else if antagonist == 2 then OneToLose
            else AntagonistWin
        else if protagonist == 1
            then if antagonist == 0 then OneProtagonist
            else if antagonist == 1 then TwoOfDifferentKind
            else Stuck
        else if protagonist == 2
            then if antagonist == 0 then OneToWin
            else Stuck
        else ProtagonistWin

opposite_token : Token -> Token
opposite_token token =
    case token of
        X -> O
        O -> X

count_of_kind : Token -> Row -> Int
count_of_kind token row =
    length <| filter
        (\field -> case field.token of
            Nothing -> False
            Just tkn -> tkn == token
        )
        row

is_field_of_kind : Token -> Field -> Bool
is_field_of_kind token field =
    case field.token of
        Nothing -> False
        Just content ->
            content == token

--Helpers--
map_item : List a -> Int -> (a -> a) -> List a
map_item list x mapper =
    let
        item_mapper i a =
            if i == x then mapper a
            else a
    in
        indexedMap item_mapper list

map_nested_item : List (List a) -> Int -> Int -> (a -> a) -> List (List a)
map_nested_item list x y mapper =
    let
        item_mapper i a =
            if i == y then map_item a x mapper
            else a
    in
        indexedMap item_mapper list

map_nested_criteria : List (List a) -> (Int -> Int -> a -> a) -> List (List a)
map_nested_criteria list mapper =
    let
        item_mapper y a =
            indexedMap (flip(mapper) y) a
    in
        indexedMap item_mapper list

nested_indexed_map : List (List a) -> (Int -> Int -> a -> Maybe b) -> List b
nested_indexed_map list mapper =
    let
        item_mapper y a =
            indexedMap (flip(mapper) y) a
    in
        filterMap identity <| concat (indexedMap item_mapper list)

--STYLES--
style_text = """
.test-app, .test-app *{
    box-sizing: border-box;
}

.test-app{
    width: 300px;
    height: 300px;
    display: block;
}
.test-app > .playground{
    display: block;
    width: 100%;
    height: 100%;
}
.test-app > .playground > .row{
    display: block;
    width: 100%;
    height: 100px;
}
.test-app > .playground > .row > .field{
    display: block;
    width: 100px;
    height: 100px;
    float: left;
    font-size: 45px;
    border: 1px solid #000;
}
.test-app > .playground > .row > .field.win{
    color: red;
}
"""
style_tag =
    Html.node "style" [type' "text/css"] [text style_text]

--VIEW--
create_row_view : Model -> Int -> Row -> Html.Html Msg
create_row_view model y row =
    let
        field_with_coord : Int -> Field -> (Int, Int, Field)
        field_with_coord x field = (x, y, field)
        fields = map (create_field_view model) (indexedMap field_with_coord row)
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


view : Model -> Html.Html Msg
view model =
    div
        [ class "test-app" ]
        [style_tag,
         div
            [class "playground"]
            (indexedMap (create_row_view model) model.playground)
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
