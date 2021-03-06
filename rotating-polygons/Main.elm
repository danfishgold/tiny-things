module Main exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g, defs, circle, line)
import Svg.Attributes exposing (width, height, cx, cy, r, x1, y1, x2, y2, strokeWidth, fill, opacity)
import Gradient exposing (gradient, gradientStroke)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Random
import AnimationFrame
import Random.Extra
import Keyboard exposing (KeyCode)


type alias Model =
    { width : Float
    , height : Float
    , t : Float
    , history : List Float
    , historyCount : Int
    , historyCycle : Float
    , vertices : List Vertex
    }


type Msg
    = Tick Float
    | SetVertices (List Vertex)
    | Key KeyCode


type alias Vertex =
    { cx : Float
    , cy : Float
    , r : Float
    , w : Float
    , phase : Float
    }


init : Int -> Float -> Int -> Float -> Float -> ( Model, Cmd Msg )
init historyCount historyCycle vertexCount width height =
    ( { width = width
      , height = height
      , t = 0
      , history = []
      , historyCount = historyCount
      , historyCycle = historyCycle
      , vertices = []
      }
    , randomizeVertices width height vertexCount
    )


randomizeVertices : Float -> Float -> Int -> Cmd Msg
randomizeVertices wd ht n =
    let
        radius =
            Random.float (min wd ht / 6) (min wd ht / 4)

        x r =
            Random.float (2 * r) (wd - 2 * r)

        y r =
            Random.float (2 * r) (ht - 2 * r)

        w =
            Random.float 0.001 0.002
                |> Random.andThen
                    (\absValue ->
                        Random.Extra.sample [ absValue, -absValue ]
                            |> Random.map (Maybe.withDefault absValue)
                    )

        phase =
            Random.float 0 (degrees 360)

        makeVertex r x y w ph =
            Vertex x y r w ph

        vertex =
            radius
                |> Random.andThen
                    (\r ->
                        Random.map4
                            (makeVertex r)
                            (x r)
                            (y r)
                            w
                            phase
                    )
    in
        Random.list n vertex
            |> Random.generate SetVertices



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.ups Key
        ]



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | t = model.t + dt } |> updateHistory, Cmd.none )

        SetVertices vertices ->
            ( { model | vertices = vertices }, Cmd.none )

        Key 13 ->
            ( { model | history = [], t = 0 }
            , randomizeVertices model.width model.height 9
            )

        Key _ ->
            ( model, Cmd.none )


updateHistory : Model -> Model
updateHistory ({ t, history, historyCycle, historyCount } as model) =
    let
        lastHistory =
            List.maximum history

        shouldAdd =
            case lastHistory of
                Nothing ->
                    True

                Just t0 ->
                    t - t0 > historyCycle

        newHistory =
            if shouldAdd then
                t :: history |> keepBiggest historyCount
            else
                history
    in
        if shouldAdd then
            { model | history = newHistory }
        else
            model


keepBiggest : Int -> List comparable -> List comparable
keepBiggest n xs =
    xs |> List.sort |> List.drop (List.length xs - n)



--


vertexParameters : Float -> Vertex -> ( Float, Float, Color )
vertexParameters t v =
    let
        theta =
            v.w * t + v.phase
    in
        ( v.cx + v.r * cos theta, v.cx + v.r * sin theta, Color.hsl theta 1 0.5 )


pairs : List a -> List ( a, a )
pairs xs =
    let
        withoutOverflow xs =
            case xs of
                fst :: snd :: rest ->
                    ( fst, snd ) :: withoutOverflow (snd :: rest)

                _ ->
                    []
    in
        case xs of
            fst :: rest ->
                withoutOverflow (xs ++ [ fst ])

            [] ->
                []


polygon : Float -> Float -> Float -> List Vertex -> Svg Msg
polygon t opacity_ strokeWidth_ vertices =
    let
        vertexParams =
            List.map (vertexParameters t) vertices

        edges =
            pairs vertexParams

        gradients =
            edges
                |> List.indexedMap
                    (\i edge ->
                        gradient (toString i) edge
                    )

        point ( x, y, c ) =
            Svg.circle
                [ cx <| toString x
                , cy <| toString y
                , r <| toString <| strokeWidth_ / 2
                , fill <| colorToCssRgb c
                ]
                []

        line i ( ( xa, ya, _ ), ( xb, yb, _ ) ) =
            Svg.line
                [ x1 <| toString xa
                , y1 <| toString ya
                , x2 <| toString xb
                , y2 <| toString yb
                , gradientStroke <| toString i
                , strokeWidth <| toString strokeWidth_
                ]
                []
    in
        g [ opacity <| toString opacity_ ]
            [ Svg.defs [] gradients
            , edges |> List.indexedMap line |> g []
            , vertexParams |> List.map point |> g []
            ]


historyOpacity : Float -> Model -> Float
historyOpacity t0 { t, historyCount, historyCycle } =
    1 - (t - t0) / (historyCycle * (toFloat historyCount + 1))


view : Model -> Svg Msg
view model =
    [ model.history |> List.map (\t0 -> polygon t0 (historyOpacity t0 model) 0.5 model.vertices) |> g []
    , polygon model.t 1 4 model.vertices
    ]
        |> svg [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 20 50 9 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
