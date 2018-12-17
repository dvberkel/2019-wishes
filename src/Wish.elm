module Wish exposing (main)

import Plane exposing (plane, at, heading, withTail)
import Plane.Position exposing (position)
import Plane.Compass exposing (Compass(..))
import Rendering.Html as Rendering

main =
    let
        aPlane =
            plane
                |> at (position 0 1)
                |> heading North
                |> withTail 4 [ position 1 1, position 1 0, position 0 0 ]
    in
        aPlane
            |> Plane.render
            |> Rendering.toHtml
