module Rendering exposing (Rendering, Shape(..), rendition, followedBy)

import Plane.Compass exposing (Compass)
import Plane.Position exposing (Position)


type Shape
    = Plane Compass Position
    | Tail (List Position)


type alias Rendering =
    List Shape

rendition: Shape -> Rendering
rendition shape =
    [shape]

followedBy : Rendering -> Rendering -> Rendering
followedBy last first =
    first ++ last
