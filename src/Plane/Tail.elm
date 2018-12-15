module Plane.Tail exposing (Tail, empty, fromList, push, toList)

import BoundedDeque as Queue exposing (BoundedDeque)
import Plane.Position exposing (Position)


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
