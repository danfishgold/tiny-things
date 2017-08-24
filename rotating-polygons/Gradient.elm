module Gradient exposing (gradient, gradientStroke)

import Svg exposing (Svg, linearGradient, defs, stop)
import Svg.Attributes exposing (id, x1, y1, x2, y2, stopColor, offset, gradientUnits)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)


gradient : String -> ( ( Float, Float, Color ), ( Float, Float, Color ) ) -> Svg msg
gradient name ( ( xa, ya, ca ), ( xb, yb, cb ) ) =
    let
        stops =
            [ stop [ offset "0%", stopColor <| colorToCssRgb ca ] []
            , stop [ offset "100%", stopColor <| colorToCssRgb cb ] []
            ]
    in
        linearGradient
            [ id name
            , x1 <| toString xa
            , y1 <| toString ya
            , x2 <| toString xb
            , y2 <| toString yb
            , gradientUnits "userSpaceOnUse"
            ]
            stops


gradientStroke : String -> Svg.Attribute msg
gradientStroke name =
    Svg.Attributes.stroke <| "url(#" ++ name ++ ")"
