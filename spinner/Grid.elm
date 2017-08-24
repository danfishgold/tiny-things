module Grid exposing (..)

import Array exposing (Array)


type alias Grid a =
    Array (Array a)


get : Int -> Int -> Grid a -> Maybe a
get i j grid =
    grid |> Array.get i |> Maybe.andThen (Array.get j)


set : Int -> Int -> a -> Grid a -> Grid a
set i j new grid =
    case Array.get i grid of
        Nothing ->
            grid

        Just row ->
            Array.set i (Array.set j new row) grid


map : (a -> b) -> Grid a -> Grid b
map fn grid =
    Array.map (Array.map fn) grid


indexedMap : (Int -> Int -> a -> b) -> Grid a -> Grid b
indexedMap fn grid =
    Array.indexedMap (\i -> Array.indexedMap (fn i)) grid



--


fromListList : List (List a) -> Grid a
fromListList grid =
    grid
        |> Array.fromList
        |> Array.map (Array.fromList)


repeat : Int -> Int -> a -> Grid a
repeat rows cols x =
    Array.repeat rows (Array.repeat cols x)


init : Int -> Int -> (Int -> Int -> a) -> Grid a
init rows cols fn =
    repeat rows cols (fn 0 0) |> indexedMap (\i j _ -> fn i j)


rows : Grid a -> Int
rows grid =
    Array.length grid


cols : Grid a -> Int
cols grid =
    grid
        |> Array.get 0
        |> Maybe.map (Array.length)
        |> Maybe.withDefault 0


size : Grid a -> ( Int, Int )
size grid =
    ( rows grid, cols grid )
