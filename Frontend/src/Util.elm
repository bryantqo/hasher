module Util exposing (..)

import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline as Dx
import MarkNodes exposing (NodeType(..))



type alias Location =
    { lat : Float
    , lon : Float
    }


locationDecoder : D.Decoder Location
locationDecoder =
    D.succeed Location
    |> Dx.required "lat" D.float
    |> Dx.required "lon" D.float

-- VIEW


decodeEvent : D.Decoder EventNode
decodeEvent =
    D.succeed EventNode
    |> Dx.required "timestamp" D.int
    |> Dx.required "type" 
        ( D.string 
            |> D.andThen 
            (\v ->
                ( case v of
                    "start" ->
                        TrailStart
                    "end" ->
                        TrailEnd
                    "mark" ->
                        TrailMark
                    "intersection" ->
                        TrailIntersection
                    _ ->
                        TrailStart
                )
                |> D.succeed
            )
        )
    |> Dx.required "location" decodeLoc


encodeEvent : EventNode -> E.Value
encodeEvent node =
    E.object
        [ ( "timestamp", E.int node.timestamp )
        , ( "location", encodeLocation node.location )
        ,   ( "type"
            , E.string
                <| case node.eventType of 
                        TrailStart ->
                            "start"
                        TrailMark ->
                            "mark"
                        TrailEnd ->
                            "end"
                        TrailIntersection ->
                            "intersection"
            )
        ]

encodeEvents : List EventNode -> E.Value
encodeEvents events =
    E.list encodeEvent events

encodeLocation : Location -> E.Value
encodeLocation loc =
    E.object
        [ ( "lat", E.float loc.lat )
        , ( "lon", E.float loc.lon )
        ]

encodeMarks : Node -> E.Value
encodeMarks node =
    case node of
        Start loc children ->
            E.object
                [ ( "type", E.string "start" )
                , ( "loc", encodeLocation loc )
                , ( "children", Maybe.map encodeMarks children |> Maybe.withDefault E.null )
                ]
        Mark loc children ->
            E.object
                [ ( "type", E.string "mark" )
                , ( "loc", encodeLocation loc )
                , ( "children", Maybe.map encodeMarks children |> Maybe.withDefault E.null )
                ]
        Intersection loc children ->
            E.object
                [ ( "type", E.string "intersection" )
                , ( "loc", encodeLocation loc )
                , ( "children", case children of
                    Nothing ->
                        E.null
                    Just list ->
                        list
                        |> E.list encodeMarks
                    )
                ]
        End loc ->
            E.object
                [ ( "type", E.string "end" )
                , ( "loc", encodeLocation loc )
                ]

type alias EventNode =
    { timestamp : Int
    , eventType : EventType
    , location : Location
    }

type EventType
    = TrailStart
    | TrailMark
    | TrailIntersection
    | TrailEnd

type Node
    = Start Location (Maybe Node)
    | End Location
    | Mark Location (Maybe Node)
    | Intersection Location ( Maybe (List Node) )


decodeMarks : D.Value -> Result D.Error (Maybe Node)
decodeMarks val =
    D.decodeValue markDecoder val


decodeLoc : D.Decoder Location
decodeLoc =
    D.succeed Location
    |> Dx.required "lat" D.float
    |> Dx.required "lon" D.float



markDecoder : D.Decoder (Maybe Node)
markDecoder =
    D.oneOf
        [ D.field "loc" decodeLoc
            |> D.andThen
            ( \loc ->
                D.field "type" D.string
                |> D.andThen
                    (\v ->  
                        case v of
                            "start" ->
                                D.field "children" markDecoder
                                |> D.andThen
                                    (\child ->
                                        D.succeed
                                        (Just <| Start loc child)
                                    )
                            "mark" ->
                                D.field "children" markDecoder
                                |> D.andThen
                                    (\child ->
                                        D.succeed
                                        (Just <| Mark loc child)
                                    )
                            "intersection" ->
                                D.field "children" 
                                    ( D.list  markDecoder 
                                            |> D.andThen 
                                                (\errs -> 
                                                    List.filterMap identity errs |> D.succeed
                                                )
                                     
                                        |> D.andThen
                                            (\child ->
                                                D.succeed
                                                (Just <| Intersection loc (Just child))
                                            )
                                        )
                                    
                            "end" ->
                                D.succeed <| Just <| End loc
                            _ ->
                                D.succeed Nothing
                    )
            )
        , D.null (Nothing)
        ]