module World exposing (World, placePlane, rewardAt, tick, world)

import Plane exposing (Plane)
import Plane.Position exposing (Position)


type World
    = World
        { width : Int
        , height : Int
        , plane : Maybe Plane
        , rewards : List Position
        }


world : Int -> Int -> World
world width height =
    World
        { width = width
        , height = height
        , plane = Nothing
        , rewards = []
        }


placePlane : Plane -> World -> World
placePlane aPlane (World aWorld) =
    World { aWorld | plane = Just aPlane }


rewardAt : Position -> World -> World
rewardAt position (World aWorld) =
    World { aWorld | rewards = position :: aWorld.rewards }


tick : World -> World
tick (World ({ plane } as aWorld)) =
    let
        nextPlane =
            plane
                |> Maybe.map Plane.move
    in
    World { aWorld | plane = nextPlane }
