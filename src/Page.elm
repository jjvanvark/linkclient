module Page exposing (Msg, Page, blank, changeRouteTo, mapDocument, toRoute, update, view)

import Browser exposing (Document)
import Effect exposing (Effect)
import Html exposing (Html)
import Page.Blank as Blank
import Page.Home as Home
import Page.NotFound as NotFound
import Route exposing (Route(..))


type Page
    = Blank
    | NotFound
    | Home Home.Model


toRoute : Page -> Route
toRoute page =
    case page of
        Blank ->
            Route.Home

        NotFound ->
            Route.Home

        Home _ ->
            Route.Home


blank : Page
blank =
    Blank


type Msg
    = GotHomeMsg Home.Msg


update : Msg -> Page -> ( Page, Effect Msg )
update message page =
    case ( message, page ) of
        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg

        ( _, _ ) ->
            ( page, Effect.none )


changeRouteTo : Maybe Route -> Page -> ( Page, Effect Msg )
changeRouteTo maybeRoute page =
    case maybeRoute of
        Nothing ->
            ( NotFound, Effect.none )

        Just Route.Home ->
            Home.init
                |> updateWith Home GotHomeMsg


updateWith :
    (pageModel -> Page)
    -> (pageMsg -> Msg)
    -> ( pageModel, Effect pageMsg )
    -> ( Page, Effect Msg )
updateWith toPage toMsg ( pageModel, effect ) =
    ( toPage pageModel
    , Effect.map toMsg effect
    )


view : Page -> Document Msg
view page =
    let
        viewPage toPageMsg config =
            viewDocument page config
                |> mapDocument toPageMsg
    in
    case page of
        Blank ->
            viewDocument page Blank.view

        NotFound ->
            viewDocument page NotFound.view

        Home home ->
            viewPage GotHomeMsg (Home.view home)


viewDocument : Page -> { title : String, content : Html msg } -> Document msg
viewDocument page { title, content } =
    { title = title
    , body = [ content ]
    }


mapDocument : (msg1 -> msg2) -> Document msg1 -> Document msg2
mapDocument changeMsg { title, body } =
    { title = title, body = List.map (Html.map changeMsg) body }
