module Main exposing (..)

import Html exposing (Html, beginnerProgram, div, button, text)
import Html.Events exposing (onClick)
import Color exposing (black)
import Pointer
import Collage exposing (..)
import Collage.Render exposing (svgBox)
import Point exposing (Point)
import Joint exposing (Joint)
import Fabrik
import Util exposing (mapPairs)


type alias Model =
    { joints : List Joint
    , target : Point.Point
    , width : Float
    , height : Float
    }


type Msg
    = SetTarget Point.Point
    | SetJoints (List Joint)


init : Float -> Float -> Model
init wd ht =
    { joints =
        [ Joint 50 0, Joint 70 0.8, Joint 30 1.2, Joint 45 0.5 ]
    , target = ( 0, 0 )
    , width = wd
    , height = ht
    }


pointerToCollage : Model -> Point.Point -> Point.Point
pointerToCollage { width, height } ( x, y ) =
    ( x - width / 2, -y + height / 2 )


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTarget position ->
            let
                target =
                    Point.capAtRadius (Joint.maxRadius model.joints) position
            in
                { model
                    | target = position
                    , joints =
                        model.joints
                            |> Fabrik.iteration position
                            |> Fabrik.iteration position
                }

        SetJoints joints ->
            { model | joints = joints }


svgView : Model -> Html Msg
svgView model =
    let
        points =
            model.joints
                |> Joint.edges

        lines =
            points
                |> mapPairs segment
                |> List.map (traced (solid thin (uniform black)))
                |> group

        dots =
            points
                |> List.map (\p -> circle 3 |> filled (uniform black) |> shift p)
                |> group

        target =
            circle 3 |> filled (uniform Color.red) |> shift model.target

        bg =
            rectangle model.width model.height |> filled (uniform Color.lightGray)
    in
        group [ lines, dots, target, bg ]
            |> svgBox ( model.width, model.height )
            |> List.singleton
            |> div [ Pointer.move (pointerToCollage model >> SetTarget) ]


view : Model -> Html Msg
view model =
    div []
        [ svgView model
        , div []
            [ button
                [ onClick <| SetJoints [ Joint 50 0, Joint 70 0.8, Joint 30 1.2, Joint 45 0.5 ]
                ]
                [ text "A few joints" ]
            , button
                [ onClick <| SetJoints <| List.repeat 10 (Joint 20 0)
                ]
                [ text "A lot of small joints" ]
            , button
                [ onClick <| SetJoints <| List.repeat 40 (Joint 20 0)
                ]
                [ text "Too many small joints" ]
            , button
                [ onClick <| SetJoints <| List.repeat 100 (Joint 2 0)
                ]
                [ text "Way too many tiny joints" ]
            ]
        ]


main : Program Never Model Msg
main =
    beginnerProgram
        { model = init 500 500
        , update = update
        , view = view
        }
