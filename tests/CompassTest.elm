module CompassTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Plane.Compass as Compass exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Position"
        [ describe "opposite" <|
              ([
              (North, South)
              , (East, West)
              , (South, North)
              , (West, East)
              ]
              |> List.map (\(original, expectedOpposite) ->
                               let
                                   name =
                                       "opposite of "
                                           ++ (Compass.toString original)
                                           ++ " is "
                                           ++ (Compass.toString expectedOpposite)
                               in
                                   test name <|
                                       \_ ->
                                           opposite original
                                               |> Expect.equal expectedOpposite
                          ))

        ]
