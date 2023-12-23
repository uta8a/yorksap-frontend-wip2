module Route exposing (Route(..), matchAbout, matchCounter, matchDescription, matchHome, matchSignIn, matchTime, toRoute, toUrl)

import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((<?>), Parser, map, oneOf, parse, s, top)
import Url.Parser.Query as Query


type Route
    = Home
    | SignIn (Maybe String)
    | Counter Int
    | Time
    | About
    | Description
    | NotFound Url


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map About <| s "about"
        , map Description <| s "description"
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
