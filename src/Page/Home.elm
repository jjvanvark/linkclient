module Page.Home exposing (Model, Msg, init, update, view)

import Effect exposing (Effect)
import Graphql.Http exposing (Error)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Parser
import Link exposing (Link, Links, init)
import RemoteData exposing (RemoteData(..))
import Route
import Start exposing (ContentLink, LinkConnection)


type alias Model =
    { links : RemoteData (Error LinkConnection) Links
    , contentLink : RemoteData (Error ContentLink) ContentLink
    , modal : Bool
    }


init : ( Model, Effect Msg )
init =
    ( Model (Loading Nothing) NotAskesd False, Effect.linker ReceivedLinker )


type Msg
    = ReceivedLinker (Result (Error LinkConnection) LinkConnection)
    | OpenLink Link
    | CloseLink
    | ReceivedContentLink (Result (Error (Maybe ContentLink)) (Maybe ContentLink))


update : Msg -> Model -> ( Model, Effect Msg )
update message model =
    case message of
        ReceivedLinker (Err err) ->
            ( { model | links = Failure err }, Effect.logout )

        ReceivedLinker (Ok result) ->
            ( { model | links = Success (Link.init result) }, Effect.none )

        OpenLink link ->
            ( { model | contentLink = Loading Nothing }, Effect.contentLink link.id ReceivedContentLink )

        CloseLink ->
            ( { model | modal = False }, Effect.none )

        ReceivedContentLink (Err err) ->
            ( model, Effect.logout )

        ReceivedContentLink (Ok maybeLink) ->
            case maybeLink of
                Nothing ->
                    ( model, Effect.none )

                Just link ->
                    ( { model | modal = True, contentLink = Success link }, Effect.none )


view : Model -> { title : String, content : Html Msg }
view { links, modal, contentLink } =
    { title = "Home"
    , content =
        div
            []
            (reader modal contentLink
                :: (case links of
                        Success lnks ->
                            [ cards False lnks ]

                        Loading maybeLnks ->
                            case maybeLnks of
                                Nothing ->
                                    []

                                Just lnks ->
                                    [ cards True lnks ]

                        otherwise ->
                            []
                   )
            )
    }


cards : Bool -> Links -> Html Msg
cards loading { links } =
    div
        [ class "grd" ]
        [ div
            [ class "grd__row" ]
            (List.map
                (\x ->
                    div [ class "grd__col grd__col--12 grd__col--sm-6 grd__col--md-4 grd__col--lg-3" ] [ card x ]
                )
                links
            )
        ]


card : Link -> Html Msg
card ({ id, title, url, favicon, image, cursor } as link) =
    article
        [ class "card", onClick <| OpenLink link ]
        [ div
            ([ class "card__image" ]
                ++ backgroundImage image
            )
            []
        , div
            [ class "card__content typography" ]
            [ h3 [ class "typography__headline" ] [ text title ] ]
        ]


reader : Bool -> RemoteData (Error ContentLink) ContentLink -> Html Msg
reader modal remoteContentLink =
    let
        className =
            if modal then
                " reader--open"

            else
                ""
    in
    div [ class <| "reader" ++ className, onClick CloseLink ]
        [ div [ class "reader__panel" ] (readerLink remoteContentLink)
        ]


readerLink : RemoteData (Error ContentLink) ContentLink -> List (Html Msg)
readerLink contentLink =
    case contentLink of
        Success { image, content } ->
            [ div ([ class "reader__image" ] ++ backgroundImage image) []
            , div [ class "reader__content" ] (parseHtml content)
            ]

        otherwise ->
            []


parseHtml : List Html.Parser.Node -> List (Html msg)
parseHtml =
    List.map
        (\node ->
            case node of
                Html.Parser.Text value ->
                    Html.text value

                Html.Parser.Element el atrrs children ->
                    Html.node el [] (parseHtml children)

                Html.Parser.Comment value ->
                    Html.text ""
        )


backgroundImage : Maybe Int -> List (Attribute msg)
backgroundImage maybeId =
    case maybeId of
        Just id ->
            [ style
                "background-image"
                ("url(http://localhost:9090/file/"
                    ++ String.fromInt id
                    ++ ")"
                )
            ]

        Nothing ->
            []
