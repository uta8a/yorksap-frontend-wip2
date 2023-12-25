module Route exposing (Route(..), matchHome, matchRoomId, matchSignIn, toRoute, toUrl)

import Maybe
import String exposing (fromInt)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((</>), Parser, fragment, map, oneOf, parse, s, string, top)


type Route
    = SignIn (Maybe String)
    | Home
    | NotFound Url
    | RoomId ( String, Maybe Int )


type alias Room =
    ( String, Maybe Int )


pair : String -> Maybe String -> ( String, Maybe Int )
pair a b =
    ( a, b |> Maybe.andThen String.toInt )


room : Parser (Room -> a) a
room =
    s "room" </> map pair (string </> fragment identity)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map RoomId <| room
        ]


toRoute : Url -> Route
toRoute url =
    url
        |> parse route
        |> Maybe.withDefault (NotFound url)


toUrl : Route -> String
toUrl r =
    case r of
        Home ->
            "/"

        SignIn redirect ->
            Builder.absolute [ "sign-in" ]
                (redirect
                    |> Maybe.map (Builder.string "redirect" >> List.singleton)
                    |> Maybe.withDefault []
                )

        RoomId ( roomId, phase ) ->
            "/room/"
                ++ roomId
                ++ "/"
                ++ (case phase of
                        Just p ->
                            fromInt p

                        Nothing ->
                            ""
                   )

        NotFound url ->
            Url.toString url


matchHome : Route -> Maybe ()
matchHome r =
    case r of
        Home ->
            Just ()

        _ ->
            Nothing


matchSignIn : Route -> Maybe (Maybe String)
matchSignIn r =
    case r of
        SignIn redirect ->
            Just redirect

        _ ->
            Nothing


matchRoomId : Route -> Maybe ( String, Maybe Int )
matchRoomId r =
    case r of
        RoomId p ->
            Just p

        _ ->
            Nothing
