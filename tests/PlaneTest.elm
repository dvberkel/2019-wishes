module PlaneTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Plane exposing (..)
import Plane.Compass as Compass exposing (..)
import Plane.Position exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Plane"
        [ describe "move"
            ([ ( North, ( 0, 1 ) )
             , ( East, ( 1, 0 ) )
             , ( South, ( 0, -1 ) )
             , ( West, ( -1, 0 ) )
             ]
                |> List.map
                    (\( direction, ( x, y ) ) ->
                        let
                            name =
                                "moving the plane "
                                    ++ Compass.toString direction
                                    ++ " changes location to ("
                                    ++ String.fromInt x
                                    ++ ", "
                                    ++ String.fromInt y
                                    ++ ")"
                        in
                        test name <|
                            \_ ->
                                let
                                    actual =
                                        plane
                                            |> at (position 0 0)
                                            |> heading direction
                                            |> move

                                    expected =
                                        plane
                                            |> at (position x y)
                                            |> heading direction
                                            |> withTail [ position 0 0 ]
                                in
                                actual
                                    |> Expect.equal expected
                    )
            )
        ]
