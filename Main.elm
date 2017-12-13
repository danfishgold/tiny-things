module Main exposing (..)

import Html exposing (Html, program, div, text)
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


init : Float -> Float -> ( Model, Cmd Msg )
init wd ht =
    ( { joints =
            [ Joint 50 0, Joint 70 0.8, Joint 30 1.2, Joint 45 0.5 ]
      , target = ( 0, 0 )
      , width = wd
      , height = ht
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


pointerToCollage : Model -> Point.Point -> Point.Point
pointerToCollage { width, height } ( x, y ) =
    ( x - width / 2, -y + height / 2 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTarget position ->
            let
                target =
                    Point.capAtRadius (Joint.maxRadius model.joints) position
            in
                ( { model
                    | target = position
                    , joints =
                        model.joints
                            |> Fabrik.iteration position
                            |> Fabrik.iteration position
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
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
                |> List.map (\p -> circle 4 |> filled (uniform black) |> shift p)
                |> group

        target =
            circle 4 |> filled (uniform Color.red) |> shift model.target

        bg =
            rectangle model.width model.height |> filled (uniform Color.lightGray)
    in
        group [ lines, dots, target, bg ]
            |> svgBox ( model.width, model.height )
            |> List.singleton
            |> div [ Pointer.move (pointerToCollage model >> SetTarget) ]


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
