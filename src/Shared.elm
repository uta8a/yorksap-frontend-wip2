module Shared exposing
    ( Identity
    , Msg(..)
    , Shared
    , identity
    , init
    , setIdentity
    , subscriptions
    , update
    )

import Browser.Navigation as Nav


type alias Identity =
    String


type alias Shared =
    { key : Nav.Key
    , identity : Maybe Identity
    }


type Msg
    = SetIdentity Identity (Maybe String)
    | ResetIdentity


identity : Shared -> Maybe String
identity =
    .identity


init : () -> Nav.Key -> ( Shared, Cmd Msg )
init _ key =
    ( { key = key
      , identity = Nothing
      }
    , Cmd.none
    )


update : Msg -> Shared -> ( Shared, Cmd Msg )
update msg shared =
    case msg of
        SetIdentity newIdentity redirect ->
            ( { shared | identity = Just newIdentity }
            , redirect
                |> Maybe.map (Nav.replaceUrl shared.key)
                |> Maybe.withDefault Cmd.none
            )

        ResetIdentity ->
            ( { shared | identity = Nothing }, Cmd.none )


subscriptions : Shared -> Sub Msg
subscriptions =
    always Sub.none


setIdentity : String -> Maybe String -> Msg
setIdentity =
    SetIdentity
