module Rendering.Html exposing (toHtml)

import Html exposing (Html)
import Html.Attributes as Attribute
import Plane.Compass as Compass exposing (Compass)
import Plane.Position as Position exposing (Position)
import Rendering exposing (Rendering, Shape(..))


toHtml : Rendering -> Html msg
toHtml rendering =
    Html.div [ Attribute.class "game" ] <|
        List.map shapeToHtml rendering


shapeToHtml : Shape -> Html msg
shapeToHtml shape =
    case shape of
        Plane direction location ->
            planeToHtml direction location

        Tail parts ->
            tailToHtml parts


planeToHtml : Compass -> Position -> Html msg
planeToHtml compass position =
    Html.div
        [ Attribute.classList
              [ ( "plane", True )
              , ( Compass.toString compass, True )
            ]
        ]
        [ Html.text <| Position.toString position ]


tailToHtml : List Position -> Html msg
tailToHtml parts =
    Html.div
        [ Attribute.class "tail"
        ]
    <|
        List.map tailPartToHtml parts


tailPartToHtml : Position -> Html msg
tailPartToHtml position =
    Html.div [ Attribute.class "tail-part" ]
        [ Html.text <| Position.toString position ]
