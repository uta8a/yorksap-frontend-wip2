module Pages.Description exposing (page)

import Effect exposing (Effect)
import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (href, id)
import Html.Events exposing (onClick)
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


type alias Model =
    { roomlist : RoomList
    }


type alias RoomList =
    List Room


type alias Room =
    { id : String
    , name : String
    }



-- init, update, view, subscriptions


init : () -> ( Model, Effect Shared.Msg Msg )
init _ =
    Model [] |> Effect.withNone


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg model =
    case msg of
        SetValue roomlist ->
            ( { model | roomlist = roomlist }, Effect.none )


view : Model -> View Msg
view model =
    { title = "Description"
    , body =
        div []
            [ text "This is the description page"
            , button [ onClick (SetValue [ { id = "1", name = "room1" }, { id = "2", name = "room2" } ]) ] [ text "Click me" ]
            , div [] [ text (withDefault { id = "null", name = "null" } (List.head model.roomlist)).name ]
            , renderList model.roomlist
            ]
    }



-- helper functions


renderList : List Room -> Html Msg
renderList lst =
    lst
        |> List.map (\l -> li [] [ text (l.id ++ " / " ++ l.name) ])
        |> ul []
