module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (text)
import Session exposing (Session)
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , session : Session
    }


init : Url -> Nav.Key -> ( Model, Effect Msg )
init url key =
    ( Model key Session.init, Effect.none )


type Msg
    = Ignored String
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Ignored _ ->
            ( model, Effect.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Effect.pushUrl url )

                Browser.External href ->
                    ( model, Effect.loadUrl href )

        ChangedUrl url ->
            ( model, Effect.none )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Link O Rama", body = [ text "Welcome" ] }


main : Program () Model Msg
main =
    Effect.application
        { init = init
        , view = view
        , update = update
        , ignore = Ignored
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        }
