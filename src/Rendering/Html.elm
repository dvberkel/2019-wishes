module Rendering.Html exposing (toHtml)

import Html exposing (Attribute, Html)
import Html.Attributes as Attribute exposing (attribute)
import Plane.Compass as Compass exposing (Compass)
import Plane.Position as Position exposing (Position)
import Rendering exposing (Rendering, Shape(..))


data : String -> String -> Attribute msg
data key value =
    let
        actualKey =
            "data-" ++ key
    in
    attribute actualKey value


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

        World width height ->
            worldToHtml width height

        Rewards rewards ->
            rewardsToHtml rewards


calculateSize : Int -> String
calculateSize value =
    "calc("
        ++ String.fromInt value
        ++ "*var(--cell-size)"


planeToHtml : Compass -> Position -> Html msg
planeToHtml compass position =
    Html.div
        [ Attribute.classList
            [ ( "plane", True )
            , ( Compass.toString compass, True )
            ]
        , Attribute.style "left" <| calculateSize position.x
        , Attribute.style "bottom" <| calculateSize position.y
        ]
        []


tailToHtml : List Position -> Html msg
tailToHtml parts =
    Html.div
        [ Attribute.class "tail"
        ]
    <|
        List.map tailPartToHtml parts


tailPartToHtml : Position -> Html msg
tailPartToHtml position =
    Html.div
        [ Attribute.class "tail-part"
        , Attribute.style "left" <| calculateSize position.x
        , Attribute.style "bottom" <| calculateSize position.y
        ]
        []


worldToHtml : Int -> Int -> Html msg
worldToHtml width height =
    Html.div
        [ Attribute.class "world"
        , Attribute.style "width" <| calculateSize width
        , Attribute.style "height" <| calculateSize height
        ]
        []


rewardsToHtml : List Position -> Html msg
rewardsToHtml parts =
    Html.div
        [ Attribute.class "rewards"
        ]
    <|
        List.map rewardToHtml parts


rewardToHtml : Position -> Html msg
rewardToHtml position =
    Html.div
        [ Attribute.class "reward"
        , Attribute.style "left" <| calculateSize position.x
        , Attribute.style "bottom" <| calculateSize position.y
        ]
        []
