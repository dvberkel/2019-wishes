module Plane.Position exposing (Position, position, toString)


type alias Position =
    { x : Int
    , y : Int
    }


position : Int -> Int -> Position
position x y =
    { x = x, y = y }


toString : Position -> String
toString { x, y } =
    "("
        ++ String.fromInt x
        ++ ","
        ++ String.fromInt y
        ++ ")"
