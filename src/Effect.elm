module Effect exposing
    ( Effect
    , application
    , batch
    , loadUrl
    , login
    , logout
    , map
    , none
    , pushUrl
    , starter
    )

import Api
import Api.Endpoint as Endpoint
import Browser
import Browser.Navigation as Nav
import Graphql.Http exposing (Error)
import Session exposing (Session, User)
import Start exposing (Start)
import Url exposing (Url)


type Effect msg
    = None
    | Batch (List (Effect msg))
    | PushUrl Url
    | LoadUrl String
    | Starter (Result (Error Start) Start -> msg) (Api.Graph Start)
    | UpdateSession Session


type alias Model r =
    { r
        | session : Session
        , key : Nav.Key
    }


application :
    { init : Url -> Nav.Key -> ( Model r, Effect msg )
    , view : Model r -> Browser.Document msg
    , update : msg -> Model r -> ( Model r, Effect msg )
    , ignore : String -> msg
    , subscriptions : Model r -> Sub msg
    , onUrlChange : Url -> msg
    , onUrlRequest : Browser.UrlRequest -> msg
    }
    -> Program () (Model r) msg
application config =
    Browser.application
        { init = \() url key -> config.init url key |> perform config.ignore
        , view = config.view
        , update = \msg model -> config.update msg model |> perform config.ignore
        , subscriptions = config.subscriptions
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        }


perform : (String -> msg) -> ( Model r, Effect msg ) -> ( Model r, Cmd msg )
perform ignore ( model, effect ) =
    case effect of
        None ->
            ( model, Cmd.none )

        Batch effects ->
            List.foldl (batchEffect ignore) ( model, [] ) effects
                |> Tuple.mapSecond Cmd.batch

        PushUrl url ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LoadUrl href ->
            ( model, Nav.load href )

        Starter toMsg graph ->
            ( model, Endpoint.graphqlRequest graph toMsg )

        UpdateSession session ->
            ( { model | session = session }, Cmd.none )


batchEffect : (String -> msg) -> Effect msg -> ( Model r, List (Cmd msg) ) -> ( Model r, List (Cmd msg) )
batchEffect ignore effect ( model, cmds ) =
    perform ignore ( model, effect )
        |> Tuple.mapSecond (\cmd -> cmd :: cmds)


none : Effect msg
none =
    None


batch : List (Effect msg) -> Effect msg
batch =
    Batch


map : (a -> msg) -> Effect a -> Effect msg
map changeMsg effect =
    case effect of
        None ->
            None

        Batch effects ->
            Batch (List.map (map changeMsg) effects)

        PushUrl url ->
            PushUrl url

        LoadUrl href ->
            LoadUrl href

        Starter toMsg graph ->
            Starter (toMsg >> changeMsg) graph

        UpdateSession session ->
            UpdateSession session


pushUrl : Url -> Effect msg
pushUrl =
    PushUrl


loadUrl : String -> Effect msg
loadUrl href =
    LoadUrl href


starter : (Result (Error Start) Start -> msg) -> Effect msg
starter toMsg =
    Starter toMsg Start.start


login : User -> Effect msg
login user =
    UpdateSession <|
        Session.inside user


logout : Effect msg
logout =
    UpdateSession Session.outside
