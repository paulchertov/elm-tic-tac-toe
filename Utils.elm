module Utils exposing(
    map_item, map_nested_item, map_nested_criteria, nested_indexed_map
    )
import List


map_item : List a -> Int -> (a -> a) -> List a
map_item list x mapper =
    let
        item_mapper i a =
            if i == x then mapper a
            else a
    in
        List.indexedMap item_mapper list


map_nested_item : List (List a) -> Int -> Int -> (a -> a) -> List (List a)
map_nested_item list x y mapper =
    let
        item_mapper i a =
            if i == y then map_item a x mapper
            else a
    in
        List.indexedMap item_mapper list


map_nested_criteria : List (List a) -> (Int -> Int -> a -> a) -> List (List a)
map_nested_criteria list mapper =
    let
        item_mapper y a =
            List.indexedMap (flip(mapper) y) a
    in
        List.indexedMap item_mapper list


nested_indexed_map : List (List a) -> (Int -> Int -> a -> Maybe b) -> List b
nested_indexed_map list mapper =
    let
        item_mapper y a =
            List.indexedMap (flip(mapper) y) a
    in
        List.filterMap identity <|
            List.concat (List.indexedMap item_mapper list)
