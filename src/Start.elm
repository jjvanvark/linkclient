module Start exposing (ContentLink, Link, LinkConnection, PageInfo, Start, User, contentLink, links, start)

import Api exposing (Graph)
import Graphql.Http exposing (Error)
import Graphql.Object
import Graphql.Object.Link as GqlLink
import Graphql.Object.LinkConnection as GqlLinkConnection
import Graphql.Object.LinkEdge as GqlLinkEdge
import Graphql.Object.PageInfo as GqlPageInfo
import Graphql.Object.User as GqlUser
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Query as Query
import Graphql.Scalar exposing (Id)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html.Parser
import Session exposing (Session)


type alias Start =
    { me : User
    }


type alias User =
    { id : Id
    , email : String
    , name : String
    }


type alias LinkConnection =
    { totalCount : Int
    , edges : List LinkEdge
    , pageInfo : PageInfo
    }


type alias LinkEdge =
    { cursor : String
    , node : Link
    }


type alias Link =
    { id : Id
    , title : String
    , url : String
    , favicon : Maybe Int
    , image : Maybe Int
    }


type alias PageInfo =
    { hasNextPage : Bool
    , hasPreviousPage : Bool
    }


type alias ContentLink =
    { id : Id
    , title : String
    , url : String
    , favicon : Maybe Int
    , image : Maybe Int
    , content : List Html.Parser.Node
    , byline : String
    , siteName : String
    }


start : Graph Start
start =
    SelectionSet.map Start
        (Query.me userSelection)


userSelection : SelectionSet User Graphql.Object.User
userSelection =
    SelectionSet.map3 User
        GqlUser.id
        GqlUser.email
        GqlUser.name


links : Graph LinkConnection
links =
    Query.links (\optionals -> { optionals | first = Present 20 }) linkConnectionSelection


linkConnectionSelection : SelectionSet LinkConnection Graphql.Object.LinkConnection
linkConnectionSelection =
    SelectionSet.map3 LinkConnection
        GqlLinkConnection.totalCount
        (GqlLinkConnection.edges linkEdgeSelection)
        (GqlLinkConnection.pageInfo pageInfoSelection)


linkEdgeSelection : SelectionSet LinkEdge Graphql.Object.LinkEdge
linkEdgeSelection =
    SelectionSet.map2 LinkEdge
        GqlLinkEdge.cursor
        (GqlLinkEdge.node linkSelection)


linkSelection : SelectionSet Link Graphql.Object.Link
linkSelection =
    SelectionSet.map5 Link
        GqlLink.id
        GqlLink.title
        GqlLink.url
        GqlLink.favicon
        GqlLink.image


pageInfoSelection : SelectionSet PageInfo Graphql.Object.PageInfo
pageInfoSelection =
    SelectionSet.map2 PageInfo
        GqlPageInfo.hasNextPage
        GqlPageInfo.hasPreviousPage



-- Link


contentLink : Id -> Graph (Maybe ContentLink)
contentLink id =
    Query.link (Query.LinkRequiredArguments id) contentLinkSelection


contentLinkSelection : SelectionSet ContentLink Graphql.Object.Link
contentLinkSelection =
    SelectionSet.map8 ContentLink
        GqlLink.id
        GqlLink.title
        GqlLink.url
        GqlLink.favicon
        GqlLink.image
        (GqlLink.content |> mapContentToHtml)
        GqlLink.byline
        GqlLink.siteName


mapContentToHtml : SelectionSet String Graphql.Object.Link -> SelectionSet (List Html.Parser.Node) Graphql.Object.Link
mapContentToHtml =
    SelectionSet.mapOrFail
        (\value ->
            Html.Parser.run value |> Result.mapError (\_ -> "Failed to parse content to html")
        )
