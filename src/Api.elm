module Api exposing (Graph)

import Api.Endpoint as Endpoint
import Graphql.Object
import Graphql.Object.User as GqlUser
import Graphql.Operation exposing (RootQuery)
import Graphql.Query as Query
import Graphql.Scalar exposing (Id)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)


type alias Graph a =
    SelectionSet a RootQuery
