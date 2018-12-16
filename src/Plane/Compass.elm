module Plane.Compass exposing (Compass(..), toString, opposite)


type Compass
    = North
    | East
    | South
    | West


toString : Compass -> String
toString compass =
    case compass of
        North ->
            "north"

        East ->
            "east"

        South ->
            "south"

        West ->
            "west"

opposite : Compass -> Compass
opposite compass =
    case compass of
        North ->
            South

        East ->
            West

        South ->
            North

        West ->
            East

