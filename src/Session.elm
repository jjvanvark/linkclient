module Session exposing
    ( Session
    , User
    , getUser
    , init
    , inside
    , isLoggedIn
    , isLoggedOut
    , outside
    )

import Graphql.Scalar exposing (Id)


type Session
    = Initial
    | In User
    | Out


init : Session
init =
    Initial


inside : User -> Session
inside =
    In


outside : Session
outside =
    Out


type alias User =
    { id : Id
    , email : String
    , name : String
    }



-- Helpers


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session of
        In _ ->
            True

        otherwise ->
            False


isLoggedOut : Session -> Bool
isLoggedOut session =
    case session of
        Out ->
            True

        otherwise ->
            False


getUser : Session -> Maybe User
getUser session =
    case session of
        In user ->
            Just user

        otherwise ->
            Nothing
