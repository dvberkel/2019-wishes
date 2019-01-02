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
    let
        ( width, height ) =
            rendering
                |> List.filter isWorld
                |> List.head
                |> Maybe.andThen dimensions
                |> Maybe.withDefault ( 0, 0 )
    in
    Html.div
        [ Attribute.class "game"
        , Attribute.style "width" <| calculateSize width
        , Attribute.style "height" <| calculateSize height
        ]
    <|
        List.map shapeToHtml rendering


isWorld : Shape -> Bool
isWorld shape =
    case shape of
        World _ _ _ ->
            True

        _ ->
            False


dimensions : Shape -> Maybe ( Int, Int )
dimensions shape =
    case shape of
        World width height _ ->
            Just ( width, height )

        _ ->
            Nothing


shapeToHtml : Shape -> Html msg
shapeToHtml shape =
    case shape of
        Plane direction location ->
            planeToHtml direction location

        Tail parts ->
            tailToHtml parts

        World width height score ->
            worldToHtml width height score

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


worldToHtml : Int -> Int -> Float -> Html msg
worldToHtml width height score =
    let
        blurValue =
            10 * (1.0 - score)
                |> ceiling
                |> String.fromInt

        blur =
            "blur(" ++ blurValue ++ "px)"
    in
    Html.div
        [ Attribute.class "world"
        , Attribute.style "filter" blur
        , Attribute.style "background-image" <| worldBackground score
        ]
        []

worldBackground : Float -> String
worldBackground score =
    let
        gradient =
            linearGradient score

        image =
            "url('image/wish.jpg')"
    in
        gradient ++ "," ++ image


linearGradient : Float -> String
linearGradient score =
    let
        color =
            hsla score
    in
    "linear-gradient(" ++ color ++ "," ++ color ++ ")"


hsla : Float -> String
hsla score =
    let
        value =
            1.0
                - score
                |> String.fromFloat
    in
    "hsla(0, 0%, 35%," ++ value ++ ")"


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
