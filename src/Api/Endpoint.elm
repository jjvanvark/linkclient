module Api.Endpoint exposing (Endpoint, graphqlRequest, login, request)

import Graphql.Http exposing (Error)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Http exposing (Body, Expect, Header)
import Url.Builder exposing (QueryParameter)



-- Endpoint


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint string) =
    string



-- Request


request :
    { method : String
    , headers : List Header
    , url : Endpoint
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
request config =
    Http.riskyRequest
        { method = config.method
        , headers = config.headers
        , url = unwrap config.url
        , body = config.body
        , expect = config.expect
        , timeout = config.timeout
        , tracker = config.tracker
        }


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin "http://localhost:9090"
        paths
        queryParams
        |> Endpoint


login : Endpoint
login =
    url [ "login" ] []



-- Graphql


query : Endpoint
query =
    url [ "query" ] []


graphqlRequest : SelectionSet typeTo RootQuery -> (Result (Error typeTo) typeTo -> msg) -> Cmd msg
graphqlRequest gqlQuery result =
    gqlQuery
        |> Graphql.Http.queryRequest (unwrap query)
        |> Graphql.Http.withCredentials
        |> Graphql.Http.send result
