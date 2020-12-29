module Page.NotFound exposing (view)

import Html exposing (Html, text)


view : { title : String, content : Html msg }
view =
    { title = "Not found"
    , content = text "Not found"
    }
