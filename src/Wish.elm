module Wish exposing (main)

import Browser
import Html exposing (Html)
import Plane exposing (Plane, at, heading, plane, withTail)
import Plane.Compass exposing (Compass(..))
import Plane.Position exposing (position)
import Rendering.Html as Rendering


main : Program () Model msg
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


init : flag -> ( Model, Cmd msg)
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

view : Model -> Html msg
view model =
    model.plane
        |> Plane.render
        |> Rendering.toHtml

-- Update

update: msg -> Model -> (Model, Cmd msg)
update _ model =
    (model, Cmd.none)

-- Subscriptions

subscriptions: Model -> Sub msg
subscriptions _ =
    Sub.none
