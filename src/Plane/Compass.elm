module Plane.Compass exposing (Compass(..), toString)


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
