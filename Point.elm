module Point exposing (..)


type alias Point =
    ( Float, Float )


onLine : Point -> Point -> Float -> Point
onLine ( x1, y1 ) ( x2, y2 ) r =
    let
        t =
            atan2 (y2 - y1) (x2 - x1)
    in
        ( x1 + r * cos t, y1 + r * sin t )


fromPolar : Float -> Float -> Point
fromPolar r theta =
    ( r * cos theta, r * sin theta )


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
