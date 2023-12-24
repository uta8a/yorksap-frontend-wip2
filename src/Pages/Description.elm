module Pages.Description exposing (page)

import Debug
import Effect exposing (Effect)
import Html exposing (Html, a, button, div, li, text, ul)
import Html.Attributes exposing (href, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map, map2, string)
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
    = GotResult (Result Http.Error RoomList)


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
    ( Loading, Effect.fromCmd (Cmd.map mapHttpRawToMsg fetchRoomList) )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg _ =
    case msg of
        GotResult result ->
            case result of
                Ok roomlist ->
                    ( Success roomlist, Effect.none )

                Err _ ->
                    ( Failure, Effect.none )


view : Model -> View Msg
view model =
    { title = "Description"
    , body =
        div []
            [ text "This is the description page"
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
    = GotRoomList (Result Http.Error RoomList)


fetchRoomList : Cmd HttpRaw
fetchRoomList =
    Http.get
        { url = "/api/v1/room"
        , expect = Http.expectJson GotRoomList roomListDecoder
        }


roomListDecoder : Decoder RoomList
roomListDecoder =
    map RoomList
        (field "roomlist" (Decode.list roomDecoder))


roomDecoder : Decoder Room
roomDecoder =
    map2 Room
        (field "id" string)
        (field "name" string)


mapHttpRawToMsg : HttpRaw -> Msg
mapHttpRawToMsg raw =
    case Debug.log "raw" raw of
        GotRoomList result ->
            case result of
                Ok roomlist ->
                    GotResult (Ok roomlist)

                Err _ ->
                    GotResult (Err (Http.BadStatus 500))
