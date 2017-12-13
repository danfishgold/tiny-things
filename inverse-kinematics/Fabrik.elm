module Fabrik exposing (..)

import Point exposing (Point)
import Joint exposing (Joint)
import Util exposing (mapPairs)


type Part
    = Rod Float
    | Jnt Point


jointsToParts : Point -> List Joint -> List Part
jointsToParts ( x0, y0 ) joints =
    case joints of
        [] ->
            [ Jnt ( x0, y0 ) ]

        { length, angle } :: rest ->
            let
                nextOrigin =
                    ( x0 + length * cos angle, y0 + length * sin angle )
            in
                Jnt ( x0, y0 ) :: Rod length :: jointsToParts nextOrigin rest


jointsFromParts : List Part -> List Joint
jointsFromParts parts =
    parts
        |> List.filterMap
            (\part ->
                case part of
                    Rod _ ->
                        Nothing

                    Jnt p ->
                        Just p
            )
        |> mapPairs Joint.fromPoints


partsHalfIteration : Point -> List Part -> List Part
partsHalfIteration end parts =
    case parts of
        (Jnt p1) :: (Rod r) :: (Jnt p2) :: rest ->
            Jnt end :: Rod r :: partsHalfIteration (Point.onLine end p2 r) (Jnt p2 :: rest)

        [ Jnt p ] ->
            [ Jnt end ]

        _ ->
            Debug.crash "bad parts" parts


partsIteration : Point -> List Part -> List Part
partsIteration end parts =
    parts
        |> List.reverse
        |> partsHalfIteration end
        |> List.reverse
        |> partsHalfIteration ( 0, 0 )


iteration : Point -> List Joint -> List Joint
iteration end joints =
    joints
        |> jointsToParts ( 0, 0 )
        |> partsIteration end
        |> jointsFromParts
