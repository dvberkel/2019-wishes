module Wish exposing (main)

import Base64
import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Keyboard exposing (RawKey, downs, navigationKey)
import Keyboard.Arrows as Arrows exposing (Direction)
import Plane exposing (Plane, at, heading, move, plane, withTail)
import Plane.Compass exposing (Compass(..))
import Plane.Position exposing (Position, position)
import Random
import Rendering.Html as Rendering
import Time exposing (every)
import World exposing (World, headTo, increaseTail, placePlane, rewardAt, tick, world)
import Markdown

main : Program Flags Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { world : World
    , message : String
    }


type alias Flags =
    { width : Int
    , height : Int
    , x : Int
    , y : Int
    , delta : Int
    , message : String
    }


init : Flags -> ( Model, Cmd Message )
init flags =
    let
        width =
            flags.width

        height =
            flags.height

        aPlane =
            plane
                |> at (position flags.x flags.y)
                |> heading North

        message =
            flags.message
                |> Base64.decode
                |> Result.withDefault "Best wishes for 2019"

        aWorld =
            world flags.delta width height aPlane
    in
    ( { world = aWorld, message = message }, Random.generate Reward <| World.rewardGenerator aWorld )



-- View


view : Model -> Html Message
view model =
    let
        score =
            model.world
            |> World.score
            |> toFloat

        maximumScore =
            model.message
                |> String.length
                |> toFloat

        relativeScore =
            score / maximumScore
                |> min 1.0

        worldHtml =
            model.world
                |> World.render relativeScore
                |> Rendering.toHtml

        text =
            String.left (World.score model.world) model.message

        messageHtml =
            Markdown.toHtml [ Attribute.class "message" ] text
    in
    Html.div [ Attribute.class "wish" ]
        [ worldHtml
        , messageHtml
        ]



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
                ( aWorld, event ) =
                    tick model.world

                cmd =
                    event
                        |> Maybe.map (eventToCommand aWorld)
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | world = aWorld }, cmd )

        Key compass ->
            let
                aWorld =
                    model.world
                        |> headTo compass
            in
            ( { model | world = aWorld }, Cmd.none )

        Reward location ->
            let
                aWorld =
                    model.world
                        |> rewardAt location
                        |> increaseTail
            in
            ( { model | world = aWorld }, Cmd.none )


eventToCommand : World -> World.Event -> Cmd Message
eventToCommand aWorld event =
    case event of
        World.RewardReached ->
            Random.generate Reward <| World.rewardGenerator aWorld



-- Subscriptions


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.batch
        [ every 100 (\_ -> Tick)
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
