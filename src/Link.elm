module Link exposing (ContentLink, Link, Links, init)

import Graphql.Scalar exposing (Id)
import Start exposing (LinkConnection)


type alias Link =
    { id : Id
    , title : String
    , url : String
    , favicon : Maybe Int
    , image : Maybe Int
    , cursor : String
    }


type alias Links =
    { total : Int
    , hasNextPage : Bool
    , hasPreviousPage : Bool
    , links : List Link
    }


init : LinkConnection -> Links
init { totalCount, edges, pageInfo } =
    Links totalCount pageInfo.hasNextPage pageInfo.hasPreviousPage (List.map fromEdge edges)


fromEdge : { cursor : String, node : Start.Link } -> Link
fromEdge { cursor, node } =
    Link node.id node.title node.url node.favicon node.image cursor



-- Content Link


type alias ContentLink =
    { id : Id
    , title : String
    , url : String
    , favicon : Maybe Int
    , image : Maybe Int
    , cursor : String
    , content : String
    , byline : String
    , siteName : String
    }
