module Page.Blank exposing (view)

import Html exposing (Html, text)


view : { title : String, content : Html msg }
view =
    { title = ""
    , content = Html.text "Blank"
    }
