module Main exposing (..)

import Html exposing (Html, div, program)
import Html.Attributes exposing (style)
import Graphics.Render exposing (..)
import AnimationFrame
import Color


type alias Model =
    { rows : Int
    , cols : Int
    , time : Float
    }


type Msg
    = Tick Float


init : Int -> Int -> Model
init rows cols =
    { rows = rows
    , cols = cols
    , time = 0
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            { model | time = model.time + dt }


mod : Float -> Float -> Float
mod y x =
    if x >= 0 then
        x - y * toFloat (floor (x / y))
    else
        -(mod y (-x))


view : Float -> Float -> Model -> Html Msg
view width height { rows, cols, time } =
    let
        freq i j =
            -- 0.00001 * toFloat (cols * i + j - i * i + 1)
            -- 0.0001 * sqrt (1 + (toFloat i + 0.5 - toFloat rows / 2) ^ 2 + (toFloat j + 0.5 - toFloat cols / 2) ^ 2)
            0.0001 * (1 + abs (toFloat i + 0.5 - toFloat rows / 2) + abs (toFloat j + 0.5 - toFloat cols / 2))

        phase i j =
            -- 0
            freq i j * time |> mod (2 * pi)

        initialPhase i j =
            atan2 ((toFloat i + 0.5) / toFloat rows - 0.5) ((toFloat j + 0.5) / toFloat cols - 0.5)

        len =
            min (width / toFloat cols) (height / toFloat rows)

        line i j =
            solidLine 2
                (solid <| color i j)
                (segment ( 0, -len / 2 ) ( 0, len / 2 ))
                |> angle (phase i j + initialPhase i j)
                |> position ( (toFloat j + 0.5) * len, (toFloat i + 0.5) * len )

        aCircle i j =
            circle (len / 2)
                |> filled (solid <| color i j)
                |> position ( (toFloat j + 0.5) * len, (toFloat i + 0.5) * len )

        color i j =
            Color.hsl (phase i j + initialPhase i j) 0.2 0.5

        row i =
            List.range 0 cols |> List.map (line i)

        grid =
            List.range 0 rows |> List.concatMap row
    in
        div
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                ]
            ]
            [ div [ style [ ( "padding", "50px 0" ) ] ]
                [ group grid |> svg 0 0 width height
                ]
            ]


main : Program Never Model Msg
main =
    program
        { init = ( init 20 20, Cmd.none )
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view 500 500
        }
