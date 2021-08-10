module MarkNodes exposing(..)


type Node =
    Node NodeType (Maybe Node)

type NodeType
    = Start
    | End
    | Mark
    | Intersection