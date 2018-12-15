module PlaneTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Plane exposing (..)
import Plane.Position exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Plane"
        [ describe "move"
            [ test "moving the plane changes position" <|
                \_ ->
                    let
                        actual =
                            plane
                                |> at (position 0 0)
                                |> heading North
                                |> move

                        expected =
                            plane
                                |> at (position 0 1)
                                |> heading North
                                |> withTail [ position 0 0 ]
                    in
                    actual
                        |> Expect.equal expected
            ]
        ]
