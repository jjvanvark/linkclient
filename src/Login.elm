module Login exposing (Model, Msg, init, update, view)

import Effect exposing (Effect)
import Graphql.Http exposing (Error)
import Html exposing (..)
import Html.Events exposing (onClick)
import Start exposing (Start, start)


type alias Model =
    { email : String
    , password : String
    , disabled : Bool
    }


init : ( Model, Effect Msg )
init =
    ( Model "" "" False, Effect.none )


type Msg
    = SetEmail String
    | SetPassword String
    | Check


update : Msg -> Model -> ( Model, Effect Msg )
update message model =
    case message of
        SetEmail email ->
            ( { model | email = email }, Effect.none )

        SetPassword password ->
            ( { model | password = password }, Effect.none )

        Check ->
            ( model, Effect.logout )


view : Model -> Html Msg
view _ =
    button [ onClick Check ] [ text "Click" ]
