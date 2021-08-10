module Pages.Map.TrailId_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.Map.TrailId_ exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Page

import Storage
import MapPorts
import Http

import Task
import Process

import Html.Attributes as Attributes

import Element as El
import Element.Border as Border
import Element.Input as Input
import Element.Background as Bg

import Json.Decode as D
import Json.Decode.Pipeline as Dx
import Http

import Util exposing(..)

import TickTock
import Html exposing (th)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init req.params
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { lastLocation : Location
    , trailId: String
    , events : List EventNode
    , tickTime : Int
    , autoMark : Bool
    }


strDec = "{\"type\":\"start\",\"loc\":{\"lat\":-8624991.945443876,\"lon\":4502351.883005126},\"children\":{\"type\":\"mark\",\"loc\":{\"lat\":-8624959.150721889,\"lon\":4502365.106303543},\"children\":{\"type\":\"mark\",\"loc\":{\"lat\":-8624885.40155924,\"lon\":4502357.646285844},\"children\":{\"type\":\"mark\",\"loc\":{\"lat\":-8618199.37483101,\"lon\":4512438.77090068},\"children\":{\"type\":\"mark\",\"loc\":{\"lat\":-8619032.734802986,\"lon\":4513456.5203788},\"children\":{\"type\":\"mark\",\"loc\":{\"lat\":-8620853.654505586,\"lon\":4513845.1823161645},\"children\":{\"type\":\"mark\",\"loc\":{\"lat\":-8621350.551316641,\"lon\":4512933.624594729},\"children\":{\"type\":\"intersection\",\"loc\":{\"lat\":-8621350.039246982,\"lon\":4512932.950831069},\"children\":[{\"type\":\"mark\",\"loc\":{\"lat\":-8620928.194036622,\"lon\":4512805.736606871},\"children\":{\"type\":\"mark\",\"loc\":{\"lat\":-8620789.790513719,\"lon\":4512691.437425473},\"children\":{\"type\":\"end\",\"loc\":{\"lat\":-8617705.216479428,\"lon\":4512206.799175553}}}}]}}}}}}}}"



init : Params -> ( Model, Effect Msg )
init params =
    (   { lastLocation = 
                { lat = 0
                , lon = 0 
                }
        , trailId = params.trailId
        , events = [] 
        , tickTime = 0
        , autoMark = True
        }
    , Cmd.batch
        [ MapPorts.initMap ()
        , TickTock.startTicking ()
        , Process.sleep
            1000
            |> ( Task.perform <| always <| LoadLocal params.trailId )
        ]
        |> Effect.fromCmd
    )




-- UPDATE


type Msg
    = NoOp
    | LoadedRemote (Result Http.Error ManyMark)
    | LoadLocal String
    | LoadedLocal (Maybe Node)
    | ErrorLoadedLocal

    | LocationChange Location
    | AddMark
    | AddInter
    | AddEnd
    | Store
    | Clean
    | Zoom

    | Transmit
    | TransmitResult (Result Http.Error TransmitResponse)

    | ShowEvents

    | Tick Int

locationDelta : Location -> Location -> Float
locationDelta a b=
    let
        dlon = a.lon - b.lon
        dlat = a.lat - b.lat
    in 
        sqrt <| (dlon * dlon) + (dlat * dlat)

maxDelta = 30

addMark : Model -> Float -> Model
addMark model delta =
    case List.reverse model.events |> List.head of
        Just tail ->
            if locationDelta tail.location model.lastLocation > delta then
                {  model
                | events = model.events ++ [ EventNode model.tickTime TrailMark model.lastLocation ]
                }
            else
                model
        Nothing ->
            {  model
            | events = model.events ++ [ EventNode model.tickTime TrailMark model.lastLocation ]
            }

addInter : Model -> Model
addInter model =
    {  model
    | events = model.events ++ [ EventNode model.tickTime TrailIntersection model.lastLocation ]
    }

filterEventList : List EventNode -> List EventNode
filterEventList events =
    case events of
        head :: tail ->
            if filterPair head tail then
                head :: filterEventList tail
            else
                case List.head tail of
                    Just t ->
                        if head.eventType == t.eventType then
                            filterEventList tail
                        else
                            head :: filterEventList tail
                    Nothing ->
                        filterEventList tail
        _ ->
            events

            
filterPair : EventNode -> List EventNode -> Bool
filterPair head rest =
    case (List.head rest) of
        ( Just b ) ->
            filterEvent head b
        _ ->
            True

filterEvent : EventNode -> EventNode -> Bool
filterEvent a b =
    let
        delta = locationDelta a.location b.location
        _ = Debug.log "Delta between evts" delta
    in
    if delta < maxDelta then
        False
    else
        True

setTickTime : Int -> Model -> Model
setTickTime tocks model =
    { model 
    | tickTime = tocks
    }

setLocation : Location -> Model -> Model
setLocation loc model =
    { model 
    | lastLocation = loc
    }

update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Tick tocks ->
            setTickTime tocks model
            |> noEffect
        LoadLocal key ->
            ( model
            , Storage.requestItem key |> Effect.fromCmd 
            )
        LoadedLocal nodes ->
            ( unwrapOldNodes nodes model
            , Process.sleep
                1000
                |> ( Task.perform <| always <| ShowEvents)
                |> Effect.fromCmd
            )
        ErrorLoadedLocal -> --TODO: Try to get it from the remote
            ( model
            , getFromServer model.trailId
                |> Effect.fromCmd
            )
        LoadedRemote res ->
            ( case res of 
                Ok (Old val) ->
                    unwrapOldNodes val model
             
                Ok (New val) ->
                    { model
                    | events = filterEventList val
                    }
             
                Err e ->
                    model
             
            , Process.sleep
                1000
                |> ( Task.perform <| always <| ShowEvents)
                |> Effect.fromCmd
            )
                    
        AddMark ->
            addMark model 0
            |> (\nm ->
                ( nm
                , appendMarks (List.reverse nm.events |> List.head |> Maybe.map (\v -> [ v ]) |> Maybe.withDefault [] )
                    |> Effect.fromCmd
                )
            )

        AddInter ->
            addInter model
            |> (\nm ->
                ( nm
                , appendMarks (List.reverse nm.events |> List.head |> Maybe.map (\v -> [ v ]) |> Maybe.withDefault [] )
                    |> Effect.fromCmd
                )
            )
            
        AddEnd ->
            {  model
            | events = model.events ++ [ EventNode model.tickTime TrailEnd model.lastLocation ]
            }
            |> (\nm ->
                ( nm
                , appendMarks (List.reverse nm.events |> List.head |> Maybe.map (\v -> [ v ]) |> Maybe.withDefault [] )
                    |> Effect.fromCmd
                )
            )
        -- Store ->
        --     ( model
        --     , Storage.storeItem "test" (Maybe.map encodeMarks model.marks |> Maybe.withDefault E.null ) |> Effect.fromCmd 
        --     )
        Clean ->
            (   { model
                | events = []
                }
            , displayMarks []
                |> Effect.fromCmd
            )
        Zoom ->
            ( model
            , MapPorts.zoom model.lastLocation
                |> Effect.fromCmd
            )
        LocationChange loc ->
            model
            |> setLocation loc
            |> (\m -> if model.autoMark then
                    let
                        nm = addMark m maxDelta
                    in
                    ( nm
                    , appendMarks (List.reverse nm.events |> List.head |> Maybe.map (\v -> [ v ]) |> Maybe.withDefault [] )
                    |> Effect.fromCmd
                    )
                else
                    m |> noEffect                    
                )
        Transmit ->
            ( model
            , transmit model.events
                |> Effect.fromCmd
            )


        ShowEvents ->
            ( model
            , displayMarks model.events
                |> Effect.fromCmd
            )
        _ ->
            model
            |> noEffect


transmit : List EventNode -> Cmd Msg
transmit events =
    Http.post
        { url = "/api/transmit"
        , body = encodeEvents events |> Http.jsonBody
        , expect = Http.expectJson TransmitResult transmissionDecoder
        }


type alias TransmitResponse =
    { id : String
    }


transmissionDecoder : D.Decoder TransmitResponse
transmissionDecoder =
    D.succeed TransmitResponse
    |> Dx.required "id" D.string


displayMarks : List EventNode -> Cmd Msg
displayMarks events =
    MapPorts.showEvents <| encodeEvents events

appendMarks : List EventNode -> Cmd Msg
appendMarks events =
    MapPorts.appendEvents <| encodeEvents events

unwrapOldNodes : Maybe Node -> Model -> Model
unwrapOldNodes node model =
    { model
    | events = unworp node []
    }


unworp : Maybe Node -> List EventNode -> List EventNode
unworp head bod =
    case head of
        Nothing ->
            bod
        Just (Start loc next) ->
            bod ++ [ EventNode 0 TrailStart loc ] ++ unworp next []
        Just (Mark loc next) ->
            bod ++ [ EventNode 0 TrailMark loc ] ++ unworp next []
        Just (Intersection loc next) ->
            bod ++ [ EventNode 0 TrailIntersection loc ] ++ ( List.foldl (\a -> \b -> a ++ b ) [] (List.map (\v -> unworp v []) ( next |> Maybe.withDefault [] |> List.map Just )) )
        Just (End loc) ->
            bod ++ [ EventNode 0 TrailEnd loc ]


noEffect : Model -> ( Model, Effect Msg)
noEffect model =
    ( model, Effect.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ MapPorts.locationChanged handleLocationChange
        , Storage.loaded handleLoaded
        , TickTock.tick (Tick)
        ]

handleLocationChange : D.Value -> Msg
handleLocationChange value =
    case D.decodeValue locationDecoder value of
        Ok val ->
            LocationChange val
        Err ex ->
            NoOp

handleLoaded : D.Value -> Msg
handleLoaded value =
    case decodeMarks value of
        Ok val ->
            LoadedLocal val
        Err ex ->
            ErrorLoadedLocal



-- VIEW


view : Model -> View Msg
view model =
    { title = "Hasher Map"
    , body =
        [ El.el 
            [ El.htmlAttribute <| Attributes.id "map"
            , El.width El.fill
            , El.height El.fill
            , El.inFront <| overlay model
            ]
            El.none
        ]
    }

overlay : Model -> El.Element Msg
overlay model =
    El.column
        [ El.width El.shrink
        , El.height El.shrink
        , El.alignBottom
        , Border.width 1
        , Border.rounded 6
        , El.padding 5
        , El.htmlAttribute
            <| Attributes.style "z-index" "999"
        , El.spacing 5
        , Bg.color transparentBlack
        ]
        [ El.row
            [ El.spacing 5
            ]
            [ button Zoom "Z" True
            , button Transmit "T" True
            ]
        , El.row
            [ El.spacing 5
            ]
            [ button Clean "Clr" True
            ]
        , El.row
            [ El.spacing 5
            ]
            [ button AddMark "M" True
            , button AddInter "I" True
            , button AddEnd "E" True
            ]
        ]


button : Msg -> String -> Bool -> El.Element Msg
button msg title enabled =
    Input.button
        [ El.padding 5
        , Border.rounded 6
        , Border.width 1
        , El.width <| El.px 30
        , El.height <| El.px 30
        , El.centerX
        , Bg.color <| if enabled then white else transparentBlack
        ]
        { onPress = if enabled then Just msg else Nothing
        , label = El.el [ El.centerX ] <| El.text title
        }


white : El.Color
white =
    El.rgb 1 1 1

transparentBlack : El.Color
transparentBlack =
    El.rgba 0 0 0 0.25

hostAddr = "https://hasher.cloud.fndream.com"

getFromServer : String -> Cmd Msg
getFromServer key =
    Http.get
        { url = hostAddr ++ "/api/get/" ++ key
        , expect = Http.expectJson LoadedRemote manyMarkDecoder
        }

manyMarkDecoder : D.Decoder ManyMark
manyMarkDecoder =
    D.oneOf
        [ markDecoder |> D.map Old 
        , markEventDecoder |> D.map New
        ]

type ManyMark
    = Old (Maybe Node)
    | New ( List EventNode)

markEventDecoder : D.Decoder (List EventNode)
markEventDecoder =
    D.list
        decodeEvent
