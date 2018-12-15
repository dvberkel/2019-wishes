module TailTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Plane.Position as Position exposing (..)
import Plane.Tail exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Tail"
        [ describe "push"
            ([ { start = [], location = position 0 0, finish = [position 0 0] }
             , { start = [position 0 0], location = position 1 0, finish = [position 1 0, position 0 0] }
             , { start = [position 1 0, position 0 0], location = position 2 0, finish = [position 2 0, position 1 0, position 0 0] }
             ]
                |> List.map
                    (\{ start, location, finish } ->
                        let
                            actual =
                                start
                                    |> fromList 3
                                    |> push location

                            expected =
                                fromList 3 finish

                            name =
                                "capacity 3 start = ["
                                    ++ (String.join ", " <| List.map Position.toString start)
                                    ++ "] finish = ["
                                    ++ (String.join ", " <| List.map Position.toString finish)
                                    ++ "]"
                        in
                        test name <|
                            \_ ->
                                (toList actual)
                                    |> Expect.equalLists (toList expected)
                    )
            )
        ]
