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


planeToHtml : Compass -> Position -> Html msg
planeToHtml compass position =
    Html.div
        [ Attribute.classList
            [ ( "plane", True )
            , ( Compass.toString compass, True )
            ]
        , data "x" <| String.fromInt position.x
        , data "y" <| String.fromInt position.y
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
    Html.div
        [ Attribute.class "tail-part"
        , data "x" <| String.fromInt position.x
        , data "y" <| String.fromInt position.y
        ]
        [ Html.text <| Position.toString position ]


worldToHtml : Int -> Int -> Html msg
worldToHtml width height =
    let
        text =
            [ width, height ]
                |> List.map String.fromInt
                |> String.join " "
    in
    Html.div
        [ Attribute.class "world"
        , data "width" <| String.fromInt width
        , data "height" <| String.fromInt height
        ]
        [ Html.text <| text ]


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
        , data "x" <| String.fromInt position.x
        , data "y" <| String.fromInt position.y
        ]
        [ Html.text <| Position.toString position ]
