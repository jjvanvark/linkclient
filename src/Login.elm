module Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Effect exposing (Effect)
import Graphql.Http exposing (Error)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, for, id, style, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode as Json
import Page exposing (Page)
import Route
import Session exposing (Session)
import Spring exposing (Spring)
import Start exposing (Start, start)
import StringValidation as SV


type alias Model =
    { email : String
    , password : String
    , disabled : Bool
    , help : Maybe String
    , modalY : Spring
    }


init : ( Model, Effect Msg )
init =
    ( Model ""
        ""
        False
        Nothing
        (Spring.create
            { strength = 250
            , dampness = 3.5
            }
            |> Spring.setTarget 100
            |> Spring.jumpTo 100
        )
    , Effect.none
    )


type Msg
    = SetEmail String
    | SetPassword String
    | Check
    | Animating Float
    | ReceivedAuthentication (Result Http.Error ())
    | ReceivedStart (Result (Error Start) Start)


update : Bool -> Msg -> Model -> Page -> ( Model, Effect Msg )
update open message preModel page =
    let
        model =
            preModel
    in
    case message of
        SetEmail email ->
            ( { model | email = email }, Effect.none )

        SetPassword password ->
            ( { model | password = password }, Effect.none )

        Check ->
            let
                tEmail =
                    String.trim model.email

                tPassword =
                    String.trim model.password
            in
            case
                SV.validate tEmail
                    [ SV.validation (SV.greaterThan 0) "Email should not be empty"
                    , SV.validation SV.email "Should be a valid email address"
                    ]
            of
                SV.Err err ->
                    ( { model | help = Just err }, Effect.focus "login-email" )

                SV.Ok ->
                    case
                        SV.validate tPassword
                            [ SV.validation (SV.greaterThan 3) "Password should be longer than 3 characters" ]
                    of
                        SV.Err err ->
                            ( { model | help = Just err }, Effect.focus "login-password" )

                        SV.Ok ->
                            ( { model
                                | help = Nothing
                                , disabled = True
                              }
                            , Effect.authenticate
                                ReceivedAuthentication
                                { email = tEmail, password = tPassword }
                            )

        Animating delta ->
            ( { model | modalY = Spring.animate delta model.modalY }, Effect.none )

        ReceivedAuthentication (Err err) ->
            ( { model | disabled = False, help = Just "Motherf", password = "" }, Effect.focus "login-email" )

        ReceivedAuthentication (Ok _) ->
            ( { model | help = Nothing, email = "", password = "", disabled = False }, Effect.starter ReceivedStart )

        ReceivedStart (Ok { me }) ->
            ( { model | disabled = False, modalY = Spring.setTarget 100 model.modalY }
            , Effect.batch [ Effect.login me, Effect.replaceUrl <| Page.toRoute page ]
            )

        ReceivedStart (Err err) ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions { modalY } =
    if Spring.atRest modalY then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta Animating


view : Session -> Model -> Html Msg
view session { modalY, disabled, email, password, help } =
    div
        [ class <| modalClass <| Session.isLoggedOut session ]
        [ div
            [ class "modal__background"
            , style "opacity"
                (modalY
                    |> Spring.value
                    |> (\v -> (v / -100.0) + 1.0)
                    |> String.fromFloat
                )
            ]
            []
        , div
            [ class "grd" ]
            [ div [ class "grd__row" ]
                [ div [ class "grd__col grd__col--12 grd__col--sm-8 grd__col--offset-sm-2 grd__col--md-6 grd__col--offset-md-3 grd__col--lg-4 grd__col--offset-lg-4" ]
                    [ div
                        [ class "modal__panel typography"
                        , style "top"
                            (modalY
                                |> Spring.value
                                |> String.fromFloat
                                |> (\v -> v ++ "vh")
                            )
                        ]
                        [ legend [ class "typography__title-1" ] [ text "Login" ]
                        , div [ class "input-group" ]
                            [ label [ for "login-email", class "typography__caption-2 input-group__label" ] [ text "login" ]
                            , input
                                [ type_ "email"
                                , value email
                                , onInput SetEmail
                                , onEnter Check
                                , Attr.disabled disabled
                                , id "login-email"
                                , class "input-group__input"
                                ]
                                []
                            ]
                        , div [ class "input-group" ]
                            [ label
                                [ for "login-password"
                                , class "typography__caption-2 input-group__label"
                                ]
                                [ text "password" ]
                            , input
                                [ type_ "password"
                                , value password
                                , onInput SetPassword
                                , onEnter Check
                                , Attr.disabled disabled
                                , id "login-password"
                                , class "input-group__input"
                                ]
                                []
                            ]
                        , div [] [ text <| Maybe.withDefault "" help ]
                        , div [ class "button-group" ]
                            [ button [ Attr.disabled disabled, class "btn btn--primary" ] [ text "Send" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


modalClass : Bool -> String
modalClass isVisible =
    let
        postfix =
            if isVisible then
                " modal--visible"

            else
                " what?"
    in
    "modal" ++ postfix


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)
