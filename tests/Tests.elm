module Tests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Fuzz exposing (Fuzzer, list, int, floatRange)
import Point exposing (Point)
import Joint exposing (Joint)
import Fabrik exposing (Part(..))
import Util exposing (mapPairs)


angle : Fuzzer Float
angle =
    floatRange 0 (2 * pi)


mod2pi : Float -> Float
mod2pi x =
    let
        normalized =
            x / (2 * pi)
    in
        2 * pi * (normalized - toFloat (floor normalized))


length : Fuzzer Float
length =
    floatRange 10 50


point : Fuzzer Point
point =
    Fuzz.map2 Point.fromPolar length angle


joint : Fuzzer Joint
joint =
    Fuzz.map2 Joint length angle


nonemptyList : Fuzzer a -> Fuzzer (List a)
nonemptyList fuzzer =
    Fuzz.map2 (::)
        fuzzer
        (list fuzzer)


clip : Float -> Float
clip x =
    toFloat (round (x * 10000)) / 10000


clipPart : Part -> Part
clipPart part =
    case part of
        Jnt ( x, y ) ->
            Jnt ( clip x, clip y )

        Rod len ->
            Rod (clip len)


clipJoint : Joint -> Joint
clipJoint { length, angle } =
    Joint (clip length) (clip angle)


closeEnoughParts : List Part -> List Part -> Expectation
closeEnoughParts parts1 parts2 =
    Expect.equalLists (List.map clipPart parts1) (List.map clipPart parts2)


closeEnoughJoints : List Joint -> List Joint -> Expectation
closeEnoughJoints joints1 joints2 =
    Expect.equalLists (List.map clipJoint joints1) (List.map clipJoint joints2)


closeEnoughPoints : Point -> Point -> Expectation
closeEnoughPoints ( x1, y1 ) ( x2, y2 ) =
    Expect.equal ( clip x1, clip y1 ) ( clip x2, clip y2 )


closeEnough : Float -> Float -> Expectation
closeEnough x y =
    Expect.equal (clip x) (clip y)


fabrik : Test
fabrik =
    describe "Fabrik"
        [ describe "half iteration"
            [ test "single joint" <|
                \_ ->
                    [ Jnt ( 0, 0 ), Rod 10, Jnt ( 10, 0 ) ]
                        |> List.reverse
                        |> Fabrik.partsHalfIteration ( 25, 0 )
                        |> List.reverse
                        |> closeEnoughParts [ Jnt ( 15, 0 ), Rod 10, Jnt ( 25, 0 ) ]
            , test "two joints" <|
                \_ ->
                    [ Jnt ( 0, 0 ), Rod 10, Jnt ( 10, 0 ), Rod 20, Jnt ( 30, 0 ) ]
                        |> List.reverse
                        |> Fabrik.partsHalfIteration ( 78, 0 )
                        |> List.reverse
                        |> closeEnoughParts [ Jnt ( 48, 0 ), Rod 10, Jnt ( 58, 0 ), Rod 20, Jnt ( 78, 0 ) ]
            , fuzz (nonemptyList joint) "solved case remains put" <|
                \joints ->
                    let
                        reversedParts =
                            joints
                                |> Fabrik.jointsToParts ( 0, 0 )
                                |> List.reverse
                    in
                        case List.head reversedParts of
                            Just (Jnt end) ->
                                reversedParts
                                    |> Fabrik.partsHalfIteration end
                                    |> closeEnoughParts reversedParts

                            _ ->
                                Expect.fail "last part of joints isn't a joint"
            ]
        , describe "full iteration"
            [ test "single joint" <|
                \_ ->
                    [ Jnt ( 0, 0 ), Rod 10, Jnt ( 10, 0 ) ]
                        |> Fabrik.partsIteration ( 0, 10 )
                        |> closeEnoughParts [ Jnt ( 0, 0 ), Rod 10, Jnt ( 0, 10 ) ]
            , test "two joints" <|
                \_ ->
                    [ Jnt ( 0, 0 ), Rod 10, Jnt ( 10, 0 ), Rod 20, Jnt ( 30, 0 ) ]
                        |> Fabrik.partsIteration ( 10, 20 )
                        |> closeEnoughParts [ Jnt ( 0, 0 ), Rod 10, Jnt ( 10, 0 ), Rod 20, Jnt ( 10, 20 ) ]
            , fuzz (nonemptyList joint) "solved case remains put" <|
                \joints ->
                    let
                        reversedParts =
                            joints
                                |> Fabrik.jointsToParts ( 0, 0 )
                                |> List.reverse
                    in
                        case List.head reversedParts of
                            Just (Jnt end) ->
                                reversedParts
                                    |> List.reverse
                                    |> Fabrik.partsIteration end
                                    |> closeEnoughParts (List.reverse reversedParts)

                            _ ->
                                Expect.fail "last part of joints isn't a joint"
            ]
        , describe "parts"
            [ skip <|
                fuzz (nonemptyList joint) "joints to parts to joints" <|
                    \joints ->
                        joints
                            |> Fabrik.jointsToParts ( 0, 0 )
                            |> Fabrik.jointsFromParts
                            |> closeEnoughJoints joints
            ]
        ]


points : Test
points =
    describe "point"
        [ describe "Point.onLine"
            [ fuzz point "valid point stays put" <|
                \( x, y ) ->
                    Point.onLine ( 0, 0 ) ( x, y ) (sqrt <| (x * x + y * y))
                        |> closeEnoughPoints ( x, y )
            , fuzz2 length angle "the angle stays the same" <|
                \len ang ->
                    Point.onLine ( 0, 0 ) (Point.fromPolar len ang) (len / 2)
                        |> Joint.fromPoint
                        |> .angle
                        |> mod2pi
                        |> closeEnough ang
            ]
        ]


util : Test
util =
    describe "mapPairs"
        [ test "empty on empty list" <|
            \_ ->
                Expect.equalLists [] <| mapPairs (,) []
        , test "empty on single element" <|
            \_ ->
                Expect.equalLists [] <| mapPairs (,) [ () ]
        , fuzz (list int) "shorter by one than the original list" <|
            \xs ->
                let
                    len =
                        List.length xs

                    lenPairs =
                        List.length <| mapPairs (,) xs
                in
                    if len > 1 then
                        Expect.equal len (lenPairs + 1)
                    else
                        Expect.equal lenPairs 0
        , test "function maintains order" <|
            \_ ->
                [ 1, 2 ] |> mapPairs (,) |> Expect.equal [ ( 1, 2 ) ]
        ]
