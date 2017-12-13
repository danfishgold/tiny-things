module Util exposing (..)


mapPairs : (a -> a -> b) -> List a -> List b
mapPairs fn xs =
    case xs of
        fst :: snd :: rest ->
            fn fst snd :: mapPairs fn (snd :: rest)

        _ ->
            []
