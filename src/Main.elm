module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Graphql.Http exposing (Error)
import Html exposing (text)
import Login
import Page exposing (Page)
import Route exposing (Route(..))
import Session exposing (Session)
import Start exposing (Start)
import StringValidation as V
import Url exposing (Url)


type alias Model =
    { key : Nav.Key
    , session : Session
    , login : Login.Model
    , page : Page
    }


init : Url -> Nav.Key -> ( Model, Effect Msg )
init url key =
    let
        ( loginModel, loginEffect ) =
            Login.init

        ( model, eff ) =
            changeRouteTo
                (Route.fromUrl url)
                { key = key
                , session = Session.init
                , login = loginModel
                , page = Page.blank
                }
    in
    ( model
    , Effect.batch
        [ Effect.map GotLoginMsg loginEffect
        , eff
        , Effect.starter ReceivedStart
        ]
    )


type Msg
    = Ignored String
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotPageMsg Page.Msg
    | GotLoginMsg Login.Msg
    | ReceivedStart (Result (Error Start) Start)


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

        GotPageMsg pageMsg ->
            Page.update pageMsg model.page
                |> fromPage model

        GotLoginMsg loginMsg ->
            let
                ( updatedModel, effect ) =
                    Login.update loginMsg model.login
            in
            ( { model | login = updatedModel }, Effect.map GotLoginMsg effect )

        ReceivedStart (Err err) ->
            ( model, Effect.logout )

        ReceivedStart (Ok { me }) ->
            ( model, Effect.login me )


changeRouteTo : Maybe Route -> Model -> ( Model, Effect Msg )
changeRouteTo maybeRoute model =
    Page.changeRouteTo maybeRoute model.page
        |> fromPage model


fromPage : Model -> ( Page, Effect msg ) -> ( Model, Effect Msg )
fromPage model ( page, effect ) =
    ( { model | page = page }
    , Effect.none
    )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view model =
    let
        { title, body } =
            Page.view model.page
                |> Page.mapDocument GotPageMsg
    in
    { title = "Link O Rama :: " ++ title, body = body ++ [ Html.map GotLoginMsg (Login.view model.login) ] }


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
