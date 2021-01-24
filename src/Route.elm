module Route exposing (Route(..), fromUrl, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing (Parser, oneOf, s)


type Route
    = Home


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


routeToString : Route -> String
routeToString route =
    Url.Builder.relative (routeToPieces route) []


routeToPieces : Route -> List String
routeToPieces route =
    case route of
        Home ->
            []
