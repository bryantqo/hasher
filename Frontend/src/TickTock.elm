port module TickTock exposing (..)

port startTicking : () -> Cmd msg
port stopTicking : () -> Cmd msg
port tick : (Int -> msg) -> Sub msg