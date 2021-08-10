port module MapPorts exposing (..)

import Json.Decode as Json
import Shared exposing (Msg)

port initMap : () -> Cmd msg

port setShapes : List Json.Value -> Cmd msg

port upload : () -> Cmd msg

port load : (Json.Value -> msg) -> Sub msg

port locationChanged : (Json.Value -> msg) -> Sub msg

port zoom : { lat : Float, lon : Float } -> Cmd msg

port showEvents : Json.Value -> Cmd msg
port appendEvents : Json.Value -> Cmd msg