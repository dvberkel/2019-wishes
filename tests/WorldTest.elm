module WorldTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Plane exposing (..)
import Plane.Compass exposing (Compass(..))
import Plane.Position as Position exposing (..)
import Plane.Tail exposing (..)
import Test exposing (..)
import World exposing (..)


suite : Test
suite =
    describe "World"
        [ describe "tick"
            [ test "should move the plane" <|
                \_ ->
                    let
                        planePosition =
                            position 4 3

                        rewardPosition =
                            position 7 9

                        aPlane =
                            plane
                                |> at planePosition
                                |> heading North

                        aWorld =
                            world 10 10
                                |> placePlane aPlane
                                |> rewardAt rewardPosition

                        expected =
                            aWorld
                                |> placePlane (move aPlane)
                    in
                    aWorld
                        |> tick
                        |> Expect.equal expected
            ]
        ]
