module Plane.Tail exposing (Tail, contains, debug, empty, fromList, incrementBy, push, toList)

import BoundedDeque as Queue exposing (BoundedDeque)
import Plane.Position as Position exposing (Position)


type Tail
    = Tail { capacity : Int, nodes : BoundedDeque Position }


empty : Int -> Tail
empty capacity =
    Tail
        { capacity = capacity
        , nodes = Queue.empty capacity
        }


fromList : Int -> List Position -> Tail
fromList capacity positions =
    Tail
        { capacity = capacity
        , nodes = Queue.fromList capacity positions
        }


toList : Tail -> List Position
toList (Tail tail) =
    Queue.toList tail.nodes


push : Position -> Tail -> Tail
push position (Tail tail) =
    Tail { tail | nodes = Queue.pushFront position tail.nodes }


contains : Position -> Tail -> Bool
contains position (Tail tail) =
    Queue.member position tail.nodes


debug : Tail -> String
debug (Tail tail) =
    let
        tailRepresentation =
            tail.nodes
                |> Queue.toList
                |> List.map Position.toString
                |> String.join ", "
    in
    "{"
        ++ String.fromInt tail.capacity
        ++ ";"
        ++ tailRepresentation
        ++ "}"


incrementBy : Int -> Tail -> Tail
incrementBy delta (Tail ({ capacity, nodes } as aTail)) =
    let
        newCapacity =
            capacity + delta

        newNodes =
            nodes
                |> Queue.resize (\_ -> newCapacity)
    in
    Tail { aTail | capacity = newCapacity, nodes = newNodes }
