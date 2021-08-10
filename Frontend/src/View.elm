module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Html exposing (Html)

import Element as El

type alias View msg =
    { title : String
    , body : List (El.Element msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = [ El.text str ]
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = List.map (El.map fn) view.body
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body = 
        [ El.layout 
            [ El.width El.fill, El.height El.fill ]
        <| El.column
            [ El.width El.fill, El.height El.fill ]
            view.body
        ]
    }
