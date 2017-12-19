module Main exposing (..)

import Html exposing (Html)
import Color exposing (Color)
import Random exposing (Generator)
import Collage exposing (..)
import Collage.Render


type alias Point =
    ( Float, Float )


type alias Cloud =
    { points : List Point, position : Point }


type alias Model =
    { sky : Color
    , clouds : List Cloud
    , width : Float
    , height : Float
    , cloudDiameter : Float
    , cloudPointCount : Int
    }


type alias Config a =
    { a
        | sky : Color
        , width : Float
        , height : Float
        , cloudDiameter : Float
        , cloudPointCount : Int
    }


pointAlongSquareFromFraction : Float -> Point
pointAlongSquareFromFraction f =
    let
        val =
            f - toFloat (floor f)
    in
        if 0 <= f && f < 1 then
            ( val, 0 )
        else if 1 <= f && f < 2 then
            ( 1, val )
        else if 2 <= f && f < 3 then
            ( 1 - val, 1 )
        else if 3 <= f && f <= 4 then
            ( 0, 1 - val )
        else
            Debug.crash <| "Bad fraction " ++ toString f


randomMonotonousSequence : Int -> Generator (List Float)
randomMonotonousSequence n =
    Random.list n (Random.float 0 1)
        |> Random.map (List.scanl (+) 0 >> List.drop 1 >> normalize 4)


normalize : Float -> List Float -> List Float
normalize desiredMax xs =
    case List.maximum xs of
        Nothing ->
            xs

        Just max ->
            List.map (\x -> x * desiredMax / max) xs


randomCloud : Int -> Generator Cloud
randomCloud pointCount =
    let
        points =
            randomMonotonousSequence pointCount
                |> Random.map (List.map pointAlongSquareFromFraction)

        position =
            Random.map2 (,) (Random.float -0.5 0.5) (Random.float 0 0.5)
    in
        Random.map2 Cloud points position


model : Int -> Int -> Config a -> Model
model initialSeed cloudCount config =
    let
        seed =
            Random.initialSeed initialSeed

        aCloud =
            randomCloud config.cloudPointCount

        ( clouds, _ ) =
            Random.step (Random.list cloudCount aCloud) seed
    in
        { sky = config.sky
        , clouds = clouds
        , width = config.width
        , height = config.height
        , cloudDiameter = config.cloudDiameter
        , cloudPointCount = config.cloudPointCount
        }


cloudShape : Config a -> Cloud -> Collage msg
cloudShape { width, height, cloudDiameter } cloud =
    let
        position =
            cloud.position |> \( x, y ) -> ( width * x, height * y )
    in
        cloud.points
            |> polygon
            |> filled (uniform (Color.rgba 256 256 256 0.2))
            |> scale cloudDiameter
            |> shift position


view : Model -> Html msg
view model =
    let
        sky =
            rectangle model.width model.height |> filled (uniform model.sky)

        clouds =
            model.clouds |> List.map (cloudShape model) |> group
    in
        group [ clouds, sky ] |> Collage.Render.svg


main : Html msg
main =
    model 1
        1000
        { sky = Color.lightBlue
        , width = 500
        , height = 500
        , cloudDiameter = 30
        , cloudPointCount = 6
        }
        |> view
