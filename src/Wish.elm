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
import World exposing (World, headTo, placePlane, rewardAt, tick, world)


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
    { world : World }


init : flag -> ( Model, Cmd Message )
init _ =
    let
        aPlane =
            plane
                |> at (position 30 20)
                |> heading North

        aWorld =
            world 80 50
                |> placePlane aPlane
    in
    ( { world = aWorld }, Cmd.none )



-- View


view : Model -> Html Message
view model =
    model.world
        |> World.render
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
                aWorld =
                    tick model.world
            in
            ( { world = aWorld }, Cmd.none )

        Key compass ->
            let
                aWorld =
                    model.world
                        |> headTo compass
            in
            ( { world = aWorld }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.batch
        [ every 200 (\_ -> Tick)
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
