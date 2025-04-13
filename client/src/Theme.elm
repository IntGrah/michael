module Theme exposing (backgroundTransition, green, grey2, grey3, grey5, grey6, grey9, red, white, whitesmoke, withAlpha)

import Element exposing (Color, fromRgb, rgb, toRgb)
import Html.Attributes


white : Color
white =
    rgb 1 1 1


whitesmoke : Color
whitesmoke =
    rgb 0.96 0.96 0.96


green : Color
green =
    rgb 0.68 0.87 0.64


red : Color
red =
    rgb 0.93 0.42 0.36


grey2 : Color
grey2 =
    rgb 0.2 0.2 0.2


grey3 : Color
grey3 =
    rgb 0.3 0.3 0.3


grey5 : Color
grey5 =
    rgb 0.5 0.5 0.5


grey6 : Color
grey6 =
    rgb 0.6 0.6 0.6


grey9 : Color
grey9 =
    rgb 0.9 0.9 0.9


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgba =
            toRgb color
    in
    fromRgb { rgba | alpha = alpha }


backgroundTransition : Element.Attribute msg
backgroundTransition =
    Element.htmlAttribute (Html.Attributes.style "transition" "background-color 100ms")
