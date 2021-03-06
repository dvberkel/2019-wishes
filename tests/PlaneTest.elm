module PlaneTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Plane exposing (..)
import Plane.Compass as Compass exposing (..)
import Plane.Position as Position exposing (..)
import Rendering
import Test exposing (..)


suite : Test
suite =
    describe "Plane"
        [ describe "move"
            ([ { from = ( 0, 0 ), direction = North, to = ( 0, 1 ) }
             , { from = ( 0, 0 ), direction = East, to = ( 1, 0 ) }
             , { from = ( 0, 0 ), direction = South, to = ( 0, -1 ) }
             , { from = ( 0, 0 ), direction = West, to = ( -1, 0 ) }
             , { from = ( 1, 2 ), direction = North, to = ( 1, 3 ) }
             , { from = ( 2, 3 ), direction = East, to = ( 3, 3 ) }
             , { from = ( 3, 5 ), direction = South, to = ( 3, 4 ) }
             , { from = ( 5, 8 ), direction = West, to = ( 4, 8 ) }
             ]
                |> List.map
                    (\{ from, direction, to } ->
                        let
                            start =
                                position
                                    (Tuple.first from)
                                    (Tuple.second from)

                            expectedPosition =
                                position
                                    (Tuple.first to)
                                    (Tuple.second to)

                            name =
                                "moving the plane from "
                                    ++ Position.toString start
                                    ++ " "
                                    ++ Compass.toString direction
                                    ++ " changes location to "
                                    ++ Position.toString expectedPosition
                        in
                        test name <|
                            \_ ->
                                let
                                    actual =
                                        plane
                                            |> at start
                                            |> heading direction
                                            |> move

                                    expected =
                                        plane
                                            |> at expectedPosition
                                            |> heading direction
                                            |> withTail 3 [ start ]
                                in
                                actual
                                    |> Expect.equal expected
                    )
            )
        , describe "collided"
            [ test "when plane entered its tail" <|
                \_ ->
                    let
                        aPlane =
                            plane
                                |> at (position 0 1)
                                |> heading South
                                |> withTail 4 [ position 1 1, position 1 0, position 0 0 ]
                                |> move
                    in
                    collided aPlane
                        |> Expect.true "plane should have collided"
            ]
        , describe "render"
            [ test "should render plane as last" <|
                \_ ->
                    let
                        aPlane =
                            plane
                                |> at (position 0 1)
                                |> heading South
                                |> withTail 4 [ position 1 1, position 1 0, position 0 0 ]

                        expected =
                            [ Rendering.Tail
                                [ position 1 1
                                , position 1 0
                                , position 0 0
                                ]
                            , Rendering.Plane South (position 0 1)
                            ]
                    in
                    render aPlane
                        |> Expect.equalLists expected
            ]
        ]
