module Plane exposing (Plane, at, collided, debug, heading, move, plane, render, withTail)

import Plane.Compass as Compass exposing (Compass(..))
import Plane.Position as Position exposing (Position, position)
import Plane.Tail as Tail exposing (Tail)
import Rendering exposing (Rendering)


type Plane
    = Plane
        { location : Position
        , direction : Compass
        , tail : Tail
        }


plane : Plane
plane =
    Plane
        { location = position 0 0
        , direction = North
        , tail = Tail.empty 3
        }


at : Position -> Plane -> Plane
at aPosition (Plane aPlane) =
    Plane { aPlane | location = aPosition }


heading : Compass -> Plane -> Plane
heading aDirection (Plane aPlane) =
    Plane { aPlane | direction = aDirection }


withTail : Int -> List Position -> Plane -> Plane
withTail capacity tailPositions (Plane aPlane) =
    let
        aTail =
            Tail.fromList capacity tailPositions
    in
    Plane { aPlane | tail = aTail }


move : Plane -> Plane
move (Plane ({ location, direction, tail } as aPlane)) =
    let
        newLocation =
            moveTo location direction

        newTail =
            Tail.push location tail
    in
    Plane { aPlane | location = newLocation, tail = newTail }


moveTo : Position -> Compass -> Position
moveTo { x, y } direction =
    case direction of
        North ->
            position x (y + 1)

        East ->
            position (x + 1) y

        South ->
            position x (y - 1)

        West ->
            position (x - 1) y


collided : Plane -> Bool
collided (Plane { location, tail }) =
    Tail.contains location tail


debug : Plane -> String
debug (Plane { location, direction, tail }) =
    "Plane["
        ++ Position.toString location
        ++ ","
        ++ Compass.toString direction
        ++ ","
        ++ Tail.debug tail
        ++ "]"


render : Plane -> Rendering
render (Plane { location, direction, tail }) =
    let
        tailRendering =
            tail
                |> Tail.toList
                |> Rendering.Tail
                |> Rendering.rendition

        planeRendering =
            Rendering.Plane direction location
                |> Rendering.rendition
    in
        tailRendering
            |> Rendering.followedBy planeRendering
