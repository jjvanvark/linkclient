module Api exposing (Graph, login)

import Api.Endpoint as Endpoint
import Graphql.Object
import Graphql.Object.User as GqlUser
import Graphql.Operation exposing (RootQuery)
import Graphql.Query as Query
import Graphql.Scalar exposing (Id)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Http
import Json.Encode as Encode



-- Login


login : { email : String, password : String } -> (Result Http.Error () -> msg) -> Cmd msg
login { email, password } cmdMsg =
    Endpoint.request
        { method = "POST"
        , headers = []
        , url = Endpoint.login
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "email", Encode.string email )
                    , ( "password", Encode.string password )
                    ]
        , expect = Http.expectWhatever cmdMsg
        , timeout = Nothing
        , tracker = Nothing
        }


type alias Graph a =
    SelectionSet a RootQuery
