module Main exposing (..)

import Html exposing (Html, program, div)
import Color exposing (black)
import Pointer
import Collage exposing (..)
import Collage.Render exposing (svgBox)


type alias Model =
    { joints : List Joint
    , target : Point
    , width : Float
    , height : Float
    }


type alias Joint =
    { length : Float
    , angle : Float
    }


type Msg
    = SetTarget Point


init : Float -> Float -> ( Model, Cmd Msg )
init wd ht =
    ( { joints = [ Joint 50 0, Joint 70 0.8, Joint 30 1.2 ]
      , target = ( 0, 0 )
      , width = wd
      , height = ht
      }
    , Cmd.none
    )


endPoint : Joint -> Point -> Point
endPoint { length, angle } ( x0, y0 ) =
    ( x0 + length * cos angle, y0 + length * sin angle )


edges : List Joint -> List Point
edges joints =
    List.scanl endPoint ( 0, 0 ) joints


totalRadius : List Joint -> Float
totalRadius joints =
    List.map .length joints |> List.sum


capAtRadius : Float -> Point -> Point
capAtRadius maxRad ( x, y ) =
    let
        r =
            sqrt (x * x + y * y)

        t =
            atan2 y x
    in
        if r > maxRad then
            ( maxRad * cos t, maxRad * sin t )
        else
            ( x, y )


mapPairs : (a -> a -> b) -> List a -> List b
mapPairs fn xs =
    case xs of
        fst :: snd :: rest ->
            fn fst snd :: mapPairs fn (snd :: rest)

        _ ->
            []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


pointerToCollage : Model -> Point -> Point
pointerToCollage { width, height } ( x, y ) =
    ( x - width / 2, -y + height / 2 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTarget position ->
            ( { model
                | target =
                    position
                        |> capAtRadius (totalRadius model.joints)
                        |> Debug.log "pos"
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        points =
            model.joints
                |> edges

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
    in
        group [ lines, dots, target ]
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
