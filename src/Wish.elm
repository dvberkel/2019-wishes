module Wish exposing (main)

import Browser
import Html exposing (Html)
import Keyboard exposing (RawKey, downs, navigationKey)
import Keyboard.Arrows as Arrows exposing (Direction)
import Plane exposing (Plane, at, heading, move, plane, withTail)
import Plane.Compass exposing (Compass(..))
import Plane.Position exposing (Position, position)
import Random exposing (Generator)
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
        width = 80

        height = 50

        aPlane =
            plane
                |> at (position 30 20)
                |> heading North

        aWorld =
            world width height
                |> placePlane aPlane
    in
    ( { world = aWorld }, Random.generate Reward <| rewardGenerator width height )


rewardGenerator : Int -> Int -> Generator Position
rewardGenerator width height =
    let
        widthGenerator =
            Random.int 0 width

        heightGenerator =
            Random.int 0 height
    in
    Random.map2 position widthGenerator heightGenerator



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
    | Reward Position


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

        Reward location ->
            let
                aWorld =
                    model.world
                        |> rewardAt location
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
