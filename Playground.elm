module Playground exposing(
    Playground,
    clear_playground, check_winner, is_center_empty,
    Token(..), opposite_token,
    Row, count_of_kind,
    Field, is_field_of_kind,
    FieldLine, all_lines, line_content
    )

import Utils exposing (nested_indexed_map)

--Playground--
type alias Playground = List Row


clear_playground : Playground
clear_playground =
    [ clear_row
    , clear_row
    , clear_row
    ]


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
                List.all zeros diagonal || List.all crosses diagonal
    in
        List.head <| List.filter is_winner all_lines


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
        case List.head <| nested_indexed_map playground is_empty_center of
            Just item -> True
            Nothing -> False


--Token--
type Token = X | O


opposite_token : Token -> Token
opposite_token token =
    case token of
        X -> O
        O -> X


--Row--
type alias Row = List Field


clear_row : Row
clear_row =
    [ empty_field
    , empty_field
    , empty_field
    ]


count_of_kind : Token -> Row -> Int
count_of_kind token row =
    List.length <| List.filter
        (\field -> case field.token of
            Nothing -> False
            Just tkn -> tkn == token
        )
        row


--Field--
type alias Field =
    { token: Maybe Token }


empty_field : Field
empty_field =
    {token = Nothing}


is_field_of_kind : Token -> Field -> Bool
is_field_of_kind token field =
    case field.token of
        Nothing -> False
        Just content ->
            content == token




--FieldLine--
type alias FieldLine = Int -> Int -> Bool


i_row : Int -> FieldLine
i_row i = (\x -> \y -> y == i)
i_col : Int -> FieldLine
i_col i = (\x -> \y -> x == i)
hack_diagonal : FieldLine
hack_diagonal = (\x -> \y -> 2 - x == y)
slash_diagonal : FieldLine
slash_diagonal = (\x -> \y -> y == x)


all_lines =
    List.concat[
      List.map i_row <| List.range 0 2,
      List.map i_col <| List.range 0 2,
      [hack_diagonal, slash_diagonal]
    ]


line_content : FieldLine -> Playground -> Row
line_content line playground =
    let
        in_line x y item =
            if line x y then Just item else Nothing
    in
        nested_indexed_map playground in_line
