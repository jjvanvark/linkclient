module RemoteData exposing (RemoteData(..))


type RemoteData e a
    = NotAsked
    | Loading (Maybe a)
    | Failure e
    | Success a
