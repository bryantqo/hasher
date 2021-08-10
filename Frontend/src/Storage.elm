port module Storage exposing (..)

import Json.Encode as Json
import Shared exposing (Msg)

port store : Json.Value -> Cmd msg
port load : Json.Value -> Cmd msg

port loaded : (Json.Value -> msg) -> Sub msg


storeItem : String -> Json.Value -> Cmd msg
storeItem key val =
    store
        <| Json.object
            [ ( "key", Json.string key )
            , ( "value", val )
            ]

requestItem : String -> Cmd msg
requestItem key =
    load
        <| Json.object
            [ ( "key", Json.string key )
            ]