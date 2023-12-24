module Pages.Description exposing (page)

import Effect exposing (Effect)
import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (href, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Maybe exposing (withDefault)
import Shared exposing (Shared)
import Spa.Page
import View exposing (View)


page : Shared -> Spa.Page.Page () Shared.Msg (View Msg) Model Msg
page _ =
    Spa.Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type Msg
    = SetValue RoomList


type Model
    = Failure
    | Loading
    | Success RoomList


type alias RoomList =
    { roomlist : List Room
    }


type alias Room =
    { id : String
    , name : String
    }



-- init, update, view, subscriptions


init : () -> ( Model, Effect Shared.Msg Msg )
init _ =
    ( Loading, Effect.fromCmd (Cmd.map mapHttpRawToMsg getPublicOpinion) )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg model =
    case msg of
        SetValue roomlist ->
            ( Success roomlist, Effect.none )


view : Model -> View Msg
view model =
    { title = "Description"
    , body =
        div []
            [ text "This is the description page"
            , button [ onClick (SetValue { roomlist = [ { id = "1", name = "room1" }, { id = "2", name = "room2" } ] }) ] [ text "Click me" ]
            , case model of
                Failure ->
                    div [] [ text "failed fetch" ]

                Loading ->
                    div [] [ text "Loading..." ]

                Success m ->
                    renderList m.roomlist
            ]
    }



-- helper functions


renderList : List Room -> Html Msg
renderList lst =
    lst
        |> List.map (\l -> li [] [ text (l.id ++ " / " ++ l.name) ])
        |> ul []


type HttpRaw
    = GotText (Result Http.Error String)


getPublicOpinion : Cmd HttpRaw
getPublicOpinion =
    Http.get
        { url = "https://elm-lang.org/assets/public-opinion.txt"
        , expect = Http.expectString GotText
        }


mapHttpRawToMsg : HttpRaw -> Msg
mapHttpRawToMsg raw =
    case raw of
        GotText result ->
            case result of
                Ok x ->
                    SetValue { roomlist = [ { id = "1", name = x }, { id = "2", name = "room2" } ] }

                Err _ ->
                    SetValue { roomlist = [ { id = "1", name = "room1" }, { id = "2", name = "room2" } ] }
