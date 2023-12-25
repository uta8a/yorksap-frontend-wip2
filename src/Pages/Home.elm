module Pages.Home exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (Html, a, div, h1, li, p, text, ul)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode exposing (Decoder, field, map, map2, string)
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
            [ h1 [] [ text "わくわくyorksapランドだよ！" ]
            , p [] [ text "ルーム一覧" ]
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
        |> List.map (\l -> li [] [ a [ href ("/room/" ++ l.id) ] [ text l.name ] ])
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
    case raw of
        GotRoomList result ->
            case result of
                Ok roomlist ->
                    GotResult (Ok roomlist)

                Err _ ->
                    GotResult (Err (Http.BadStatus 500))
