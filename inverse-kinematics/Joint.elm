module Joint exposing (..)

import Point exposing (Point)


type alias Joint =
    { length : Float
    , angle : Float
    }


fromPoints : Point -> Point -> Joint
fromPoints ( x1, y1 ) ( x2, y2 ) =
    { length = sqrt <| (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
    , angle = atan2 (y2 - y1) (x2 - x1)
    }


fromPoint : Point -> Joint
fromPoint =
    fromPoints ( 0, 0 )


endPoint : Joint -> Point -> Point
endPoint { length, angle } ( x0, y0 ) =
    ( x0 + length * cos angle, y0 + length * sin angle )


edges : List Joint -> List Point
edges joints =
    List.scanl endPoint ( 0, 0 ) joints


maxRadius : List Joint -> Float
maxRadius joints =
    List.map .length joints |> List.sum
