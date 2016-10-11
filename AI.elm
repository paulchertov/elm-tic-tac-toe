module AI exposing (
    LineType(..),
    map_possibilities, coordinates_of_empties_in_line,
    coordinates_of_all_empties
    )
import Playground exposing (Token(..), Playground, Row, FieldLine, Field,
    count_of_kind, opposite_token, all_lines, line_content
    )
import Utils exposing (nested_indexed_map)

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



map_possibilities : Token -> Playground -> List (FieldLine, LineType)
map_possibilities token playground =
    let
        content_extractor = flip line_content playground
        lines_content =  List.map content_extractor all_lines
    in
        List.map2 (,) all_lines <| List.map (line_type token) lines_content



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
