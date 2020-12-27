module Start exposing (Start, start)

import Api exposing (Graph)
import Graphql.Http exposing (Error)
import Graphql.Object
import Graphql.Object.PageInfo as GqlPageInfo
import Graphql.Object.Project as GqlProject
import Graphql.Object.ProjectConnection as GqlProjectConnection
import Graphql.Object.ProjectEdge as GqlProjectEdge
import Graphql.Object.User as GqlUser
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Query as Query
import Graphql.Scalar exposing (Id)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Session exposing (Session)


type alias Start =
    { me : User
    , projects : ProjectConnection
    }


type alias User =
    { id : Id
    , email : String
    , name : String
    }


type alias ProjectConnection =
    { totalCount : Int
    , edges : List ProjectEdge
    , pageInfo : PageInfo
    }


type alias ProjectEdge =
    { cursor : String
    , node : Project
    }


type alias Project =
    { id : Id
    , name : String
    , key : String
    }


type alias PageInfo =
    { hasNextPage : Bool
    , hasPreviousPage : Bool
    }


start : Graph Start
start =
    SelectionSet.map2 Start
        (Query.me userSelection)
        (Query.projects (\optionals -> { optionals | first = Present 20 }) projectsSelection)


userSelection : SelectionSet User Graphql.Object.User
userSelection =
    SelectionSet.map3 User
        GqlUser.id
        GqlUser.email
        GqlUser.name


projectsSelection : SelectionSet ProjectConnection Graphql.Object.ProjectConnection
projectsSelection =
    SelectionSet.map3 ProjectConnection
        GqlProjectConnection.totalCount
        (GqlProjectConnection.edges projectEdgeSelection)
        (GqlProjectConnection.pageInfo pageInfoSelection)


projectEdgeSelection : SelectionSet ProjectEdge Graphql.Object.ProjectEdge
projectEdgeSelection =
    SelectionSet.map2 ProjectEdge
        GqlProjectEdge.cursor
        (GqlProjectEdge.node projectSelection)


projectSelection : SelectionSet Project Graphql.Object.Project
projectSelection =
    SelectionSet.map3 Project
        GqlProject.id
        GqlProject.name
        GqlProject.key


pageInfoSelection : SelectionSet PageInfo Graphql.Object.PageInfo
pageInfoSelection =
    SelectionSet.map2 PageInfo
        GqlPageInfo.hasNextPage
        GqlPageInfo.hasPreviousPage
