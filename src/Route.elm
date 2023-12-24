module Route exposing (Route(..), matchAbout, matchCounter, matchDescription, matchHome, matchRoomId, matchSignIn, matchTime, toRoute, toUrl)

import Html exposing (a)
import Maybe exposing (withDefault)
import String exposing (fromInt)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((</>), (<?>), Parser, fragment, int, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | SignIn (Maybe String)
    | Counter Int
    | Time
    | About
    | Description
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
        , map About <| s "about"
        , map Description <| s "description"
        , map RoomId <| room
        , map SignIn <| s "sign-in" <?> Query.string "redirect"
        , map Counter <| s "counter" <?> (Query.int "value" |> Query.map (Maybe.withDefault 0))
        , map Time <| s "time"
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

        About ->
            "/about"

        Description ->
            "/Description"

        SignIn redirect ->
            Builder.absolute [ "sign-in" ]
                (redirect
                    |> Maybe.map (Builder.string "redirect" >> List.singleton)
                    |> Maybe.withDefault []
                )

        Counter value ->
            "/counter?value=" ++ String.fromInt value

        Time ->
            "/time"

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


matchAny : Route -> Route -> Maybe ()
matchAny any r =
    if any == r then
        Just ()

    else
        Nothing


matchHome : Route -> Maybe ()
matchHome =
    matchAny Home


matchAbout : Route -> Maybe ()
matchAbout r =
    case r of
        About ->
            Just ()

        _ ->
            Nothing


matchDescription : Route -> Maybe ()
matchDescription r =
    case r of
        Description ->
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


matchCounter : Route -> Maybe Int
matchCounter r =
    case r of
        Counter value ->
            Just value

        _ ->
            Nothing


matchTime : Route -> Maybe ()
matchTime =
    matchAny Time


matchRoomId : Route -> Maybe ( String, Maybe Int )
matchRoomId r =
    case r of
        RoomId p ->
            Just p

        _ ->
            Nothing
