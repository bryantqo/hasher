module Pages.Map exposing (Model, Msg, page)

import Effect exposing (Effect)
import Gen.Params.Map exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Page

import TickTock
import Http

import Task
import Process

import Html.Attributes as Attributes

import Element as El
import Element.Border as Border
import Element.Input as Input
import Element.Background as Bg

import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline as Dx

import Storage
import Util exposing(..)

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { lastLocation : Location
    , marks : Maybe Node
    }


init : ( Model, Effect Msg )
init =
    (   { lastLocation = 
                { lat = 0
                , lon = 0 
                }
        , marks = Nothing
        }
    , Cmd.batch
        [ TickTock.initMap ()
        , Process.sleep
            1000
            |> ( Task.perform <| always Load )
        ]
        |> Effect.fromCmd
    )



-- UPDATE


type Msg
    = NoOp
    | Uploaded (Result Http.Error ())
    | Load
    | Loaded (Maybe Node)
    | LocationChange Location
    | AddMark
    | AddInter
    | AddEnd
    | Store
    | Clean
    | Zoom
    | Transmit
    | TransmitResult (Result Http.Error TransmitResponse)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        LocationChange loc ->
            (   { model 
                | lastLocation = loc
                }
            , Effect.none
            )
        AddMark ->
            ( addMark model
            , Effect.none
            )
        AddInter ->
            ( addInter model
            , Effect.none
            )        
        AddEnd ->
            ( addEnd model
            , Effect.none
            )
        Store ->
            ( model
            , Storage.storeItem "test" (Maybe.map encodeMarks model.marks |> Maybe.withDefault E.null ) |> Effect.fromCmd 
            )
        Load ->
            ( model
            , Storage.requestItem "test" |> Effect.fromCmd 
            )
        Loaded marks ->
            (   { model
                | marks = marks
                }
            , Effect.none
            )
        Clean ->
            (   { model
                | marks = Nothing
                }
            , Effect.none
            )
        Zoom ->
            ( model
            , TickTock.zoom model.lastLocation
                |> Effect.fromCmd
            )
        Transmit ->
            ( model
            , transmit model.marks
                |> Effect.fromCmd
            )
        _ ->
            ( model, Effect.none )


transmit : Maybe Node -> Cmd Msg
transmit head =
    Http.post
        { url = "/api/transmit"
        , body = head |> Maybe.map encodeMarks |> Maybe.withDefault E.null |> Http.jsonBody
        , expect = Http.expectJson TransmitResult transmissionDecoder
        }

type alias TransmitResponse =
    { id : String
    }


transmissionDecoder : D.Decoder TransmitResponse
transmissionDecoder =
    D.succeed TransmitResponse
    |> Dx.required "id" D.string


addMark : Model -> Model
addMark model =
    { model 
    | marks = 
        Just
        <| case model.marks of
            Nothing ->
                Start model.lastLocation Nothing
            Just head ->
                append head (Mark model.lastLocation Nothing)
    }

addInter : Model -> Model
addInter model =
    { model 
    | marks = 
        Just
        <| case model.marks of
            Nothing ->
                Intersection model.lastLocation Nothing
            Just head ->
                append head (Intersection model.lastLocation Nothing)
    }

addEnd : Model -> Model
addEnd model =
    { model 
    | marks = 
        Just
        <| case model.marks of
            Nothing ->
                End model.lastLocation
            Just head ->
                append head (End model.lastLocation)
    }

append : Node -> Node -> Node
append head val =
    case head of
        Start loc Nothing ->
            Start loc (Just val)
        Mark loc Nothing ->
            Mark loc (Just val)
        Intersection loc Nothing ->
            Intersection loc (Just [ val ])
        Start loc (Just next) ->
            Start loc ( Just <| append next val )
        Mark loc (Just next) ->
            Mark loc ( Just <| append next val )
        Intersection loc (Just []) ->
            Intersection loc ( Just <| [ val ] )
        Intersection loc (Just (first :: rest)) ->
            Intersection loc ( Just <| (append first val) :: rest )
        End loc ->
            End loc



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ TickTock.locationChanged handleLocationChange
        , Storage.loaded handleLoaded
        ]

handleLoaded : D.Value -> Msg
handleLoaded value =
    case D.decodeValue markDecoder value of
        Ok val ->
            Loaded val
        Err ex ->
            NoOp

handleLocationChange : D.Value -> Msg
handleLocationChange value =
    case D.decodeValue locationDecoder value of
        Ok val ->
            LocationChange val
        Err ex ->
            NoOp


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
            [ button Load "L" True
            , button Store "S" True
            , button Clean "Clr" True
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