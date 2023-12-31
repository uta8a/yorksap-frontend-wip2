module Main exposing (main)

import Browser exposing (Document)
import Html exposing (button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Pages.Home as Home
import Pages.Room.Id as RoomId
import Pages.SignIn as SignIn
import Route
import Shared exposing (Shared)
import Spa
import View exposing (View)


mappers : ( (a -> b) -> View a -> View b, (c -> d) -> View c -> View d )
mappers =
    ( View.map, View.map )


toDocument :
    Shared
    -> View (Spa.Msg Shared.Msg pageMsg)
    -> Document (Spa.Msg Shared.Msg pageMsg)
toDocument shared view =
    { title = view.title
    , body =
        [ div
            [ style "font-size" "20px" ]
            [ div
                [ style "width" "100%"
                , style "height" "100%"
                ]
                [ div
                    [ style "text-align" "right"
                    , style "padding" "20px"
                    ]
                  <|
                    case shared.identity of
                        Just username ->
                            [ text username
                            , text " | "
                            , button [ onClick (Spa.mapSharedMsg Shared.ResetIdentity) ] [ text "logout" ]
                            ]

                        Nothing ->
                            []
                , div
                    [ style "display" "flex"
                    , style "align-items" "center"
                    , style "justify-content" "center"
                    ]
                    [ view.body ]
                ]
            ]
        ]
    }


main =
    Spa.init
        { defaultView = View.defaultView
        , extractIdentity = Shared.identity
        }
        |> Spa.addPublicPage mappers Route.matchHome Home.page
        |> Spa.addPublicPage mappers Route.matchSignIn SignIn.page
        |> Spa.addPublicPage mappers Route.matchRoomId RoomId.page
        |> Spa.application View.map
            { init = Shared.init
            , subscriptions = Shared.subscriptions
            , update = Shared.update
            , toRoute = Route.toRoute
            , toDocument = toDocument
            , protectPage = Route.toUrl >> Just >> Route.SignIn >> Route.toUrl
            }
        |> Browser.application
