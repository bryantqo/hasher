module Pages.Home_ exposing (view)

import Html
import View exposing (View)
import Element as El


view : View msg
view =
    { title = "Homepage"
    , body = 
        [ El.el 
            [ El.width El.fill
            , El.height El.fill
            ]
            El.none
        ]
    }
