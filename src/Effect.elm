module Effect exposing
    ( Effect
    , application
    , batch
    , loadUrl
    , map
    , none
    , pushUrl
    )

import Browser
import Browser.Navigation as Nav
import Session exposing (Session)
import Url exposing (Url)


type Effect msg
    = None
    | Batch (List (Effect msg))
    | PushUrl Url
    | LoadUrl String


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


pushUrl : Url -> Effect msg
pushUrl =
    PushUrl


loadUrl : String -> Effect msg
loadUrl href =
    LoadUrl href
