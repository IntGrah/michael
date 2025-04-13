module Vector exposing (Vector, add, scale, sub)

{- 2D Float vectors. -}


type alias Vector =
    ( Float, Float )


add : Vector -> Vector -> Vector
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


sub : Vector -> Vector -> Vector
sub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


scale : Float -> Vector -> Vector
scale k ( x, y ) =
    ( k * x, k * y )
