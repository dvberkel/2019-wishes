module Plane exposing (Compass(..), Plane, at, heading, move, plane, withTail)

import Plane.Position exposing (Position, position)
import Plane.Tail as Tail exposing (Tail)


type Plane
    = Plane
        { location : Position
        , direction : Compass
        , tail : Tail
        }


type Compass
    = North
    | East
    | South
    | West


plane : Plane
plane =
    Plane
        { location = position 0 0
        , direction = North
        , tail = Tail.empty
        }


at : Position -> Plane -> Plane
at aPosition (Plane aPlane) =
    Plane { aPlane | location = aPosition }


heading : Compass -> Plane -> Plane
heading aDirection (Plane aPlane) =
    Plane { aPlane | direction = aDirection }


withTail : List Position -> Plane -> Plane
withTail tailPositions (Plane aPlane) =
    let
        aTail =
            Tail.fromList tailPositions
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
