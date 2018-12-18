module Wish exposing (main)

import Browser
import Html exposing (Html)
import Keyboard exposing (RawKey, downs, navigationKey)
import Keyboard.Arrows as Arrows exposing (Direction)
import Plane exposing (Plane, at, heading, move, plane, withTail)
import Plane.Compass exposing (Compass(..))
import Plane.Position exposing (position)
import Rendering.Html as Rendering
import Time exposing (every)


main : Program () Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { plane : Plane }


init : flag -> ( Model, Cmd Message )
init _ =
    let
        aPlane =
            plane
                |> at (position 0 1)
                |> heading North
                |> withTail 4 [ position 1 1, position 1 0, position 0 0 ]
    in
    ( { plane = aPlane }, Cmd.none )



-- View


view : Model -> Html Message
view model =
    model.plane
        |> Plane.render
        |> Rendering.toHtml



-- Update


type Message
    = DoNothing
    | Tick
    | Key Compass


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        DoNothing ->
            ( model, Cmd.none )

        Tick ->
            let
                plane =
                    move model.plane
            in
            ( { plane = plane }, Cmd.none )

        Key compass ->
            let
                plane =
                    model.plane
                        |> heading compass
            in
            ( { plane = plane }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.batch
        [ every 1000 (\_ -> Tick)
        , downs rawKeyToMessage
        ]


rawKeyToMessage : RawKey -> Message
rawKeyToMessage rawKey =
    rawKey
        |> navigationKey
        |> Maybe.map List.singleton
        |> Maybe.map Arrows.arrowsDirection
        |> Maybe.andThen toCompass
        |> Maybe.map Key
        |> Maybe.withDefault DoNothing


toCompass : Direction -> Maybe Compass
toCompass aDirection =
    case aDirection of
        Arrows.North ->
            Just North

        Arrows.East ->
            Just East

        Arrows.South ->
            Just South

        Arrows.West ->
            Just West

        _ ->
            Nothing
