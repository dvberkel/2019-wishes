module World exposing (Event(..), World, headTo, placePlane, render, rewardAt, rewardGenerator, tick, world)

import Plane exposing (Plane)
import Plane.Compass exposing (Compass)
import Plane.Position exposing (Position, position)
import Random exposing (Generator)
import Rendering exposing (Rendering, followedBy, optionally, rendition)


type Event
    = RewardReached


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


headTo : Compass -> World -> World
headTo compass (World ({ plane } as aWorld)) =
    let
        nextPlane =
            plane
                |> Maybe.map (Plane.heading compass)
    in
    World { aWorld | plane = nextPlane }


tick : World -> ( World, Maybe Event )
tick (World ({ plane, rewards } as aWorld)) =
    let
        nextPlane =
            plane
                |> Maybe.map Plane.move
    in
    ( World { aWorld | plane = nextPlane }, Nothing )


render : World -> Rendering
render (World aWorld) =
    let
        planeRendition =
            aWorld.plane
                |> Maybe.map Plane.render
                |> optionally
    in
    Rendering.World aWorld.width aWorld.height
        |> rendition
        |> followedBy (renderRewards aWorld.rewards)
        |> followedBy planeRendition


renderRewards : List Position -> Rendering
renderRewards rewards =
    rewards
        |> Rendering.Rewards
        |> rendition


rewardGenerator : World -> Generator Position
rewardGenerator (World { width, height }) =
    let
        widthGenerator =
            Random.int 0 width

        heightGenerator =
            Random.int 0 height
    in
    Random.map2 position widthGenerator heightGenerator
