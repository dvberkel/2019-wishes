module Rendering exposing (Rendering, Shape(..), rendition, followedBy, optionally)

import Plane.Compass exposing (Compass)
import Plane.Position exposing (Position)


type Shape
    = Plane Compass Position
    | Tail (List Position)
    | World Int Int Float
    | Rewards (List Position)


type alias Rendering =
    List Shape

rendition: Shape -> Rendering
rendition shape =
    [shape]

followedBy : Rendering -> Rendering -> Rendering
followedBy last first =
    first ++ last

optionally : Maybe Rendering -> Rendering
optionally option =
    option
        |> Maybe.withDefault []
