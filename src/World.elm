module World exposing (Event(..), World, headTo, increaseTail, placePlane, render, rewardAt, rewardGenerator, score, tick, world)

import Plane exposing (Plane)
import Plane.Compass exposing (Compass)
import Plane.Position exposing (Position, position)
import Random exposing (Generator)
import Rendering exposing (Rendering, followedBy, optionally, rendition)


type Event
    = RewardReached


type World
    = World
        { delta : Int
        , width : Int
        , height : Int
        , plane : Plane
        , reward : Maybe Position
        }


world : Int -> Int -> Int -> Plane -> World
world delta width height plane =
    World
        { delta = delta
        , width = width
        , height = height
        , plane = plane
        , reward = Nothing
        }


placePlane : Plane -> World -> World
placePlane aPlane (World aWorld) =
    World { aWorld | plane = aPlane }


rewardAt : Position -> World -> World
rewardAt position (World aWorld) =
    World { aWorld | reward = Just position }


headTo : Compass -> World -> World
headTo compass (World ({ plane } as aWorld)) =
    let
        nextPlane =
            plane
                |> Plane.heading compass
    in
    World { aWorld | plane = nextPlane }


tick : World -> ( World, Maybe Event )
tick (World ({ width, height, plane, reward } as aWorld)) =
    let
        nextPlane =
            plane
                |> Plane.move
                |> Plane.wrap width height

        rewardReached =
            reward
                |> Maybe.map (Plane.on nextPlane)
                |> Maybe.withDefault False

        event =
            if rewardReached then
                Just RewardReached

            else
                Nothing
    in
    ( World { aWorld | plane = nextPlane }, event )


render : World -> Rendering
render (World aWorld) =
    let
        planeRendition =
            aWorld.plane
                |> Plane.render
    in
    Rendering.World aWorld.width aWorld.height
        |> rendition
        |> followedBy (renderReward aWorld.reward)
        |> followedBy planeRendition


renderReward : Maybe Position -> Rendering
renderReward reward =
    reward
        |> Maybe.map (\r -> [ r ])
        |> Maybe.withDefault []
        |> Rendering.Rewards
        |> rendition


rewardGenerator : World -> Generator Position
rewardGenerator (World { width, height }) =
    let
        widthGenerator =
            Random.int 0 (width - 1)

        heightGenerator =
            Random.int 0 (height - 1)
    in
    Random.map2 position widthGenerator heightGenerator


increaseTail : World -> World
increaseTail (World ({ plane, delta } as aWorld)) =
    let
        newPlane =
            plane
                |> Plane.increaseTail delta

        newDelta =
            delta + 1
    in
    World { aWorld | plane = newPlane, delta = newDelta }


score : World -> Int
score (World { plane }) =
    Plane.length plane
