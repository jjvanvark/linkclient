module Page.Home exposing (Model, Msg, init, update, view)

import Effect exposing (Effect)
import Html exposing (..)
import Html.Events exposing (onClick)
import Route


type alias Model =
    { counter : Int }


type Msg
    = AddOne


update : Msg -> Model -> ( Model, Effect Msg )
update message model =
    case message of
        AddOne ->
            ( { model | counter = model.counter + 1 }, Effect.none )


init : ( Model, Effect Msg )
init =
    ( Model 22, Effect.none )


view : Model -> { title : String, content : Html Msg }
view { counter } =
    { title = "Home"
    , content =
        div
            []
            [ text <| String.fromInt counter
            , button [ onClick AddOne ] [ text "go" ]
            ]
    }
