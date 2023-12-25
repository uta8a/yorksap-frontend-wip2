module Pages.Room.Id exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (Html, a, div, h1, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (href, style)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map, map2, map3, map4, maybe, string)
import List exposing (concat, head, singleton)
import Result exposing (Result)
import Shared exposing (Shared)
import Spa.Page
import String exposing (fromInt)
import View exposing (View)


page : Shared -> Spa.Page.Page ( String, Maybe Int ) Shared.Msg (View Msg) Model Msg
page _ =
    Spa.Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


type Msg
    = GetGameData (Result Http.Error GameData)
    | GetRoomData (Result Http.Error RoomData)


type alias MainData =
    { room : RoomData
    , game : GameData
    }


type Model
    = Failure Http.Error
    | Loading
    | Success MainData


init : ( String, Maybe Int ) -> ( Model, Effect Shared.Msg Msg )
init ( roomId, phase ) =
    ( Loading
    , Effect.fromCmd
        (Cmd.batch
            [ fetchGameData ( roomId, phase )
            , fetchRoomData roomId
            ]
        )
    )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg model =
    case msg of
        GetGameData gameResult ->
            case gameResult of
                Ok gameData ->
                    case model of
                        Success data ->
                            ( Success { room = data.room, game = gameData }, Effect.none )

                        _ ->
                            ( Success { room = { roomName = "" }, game = gameData }, Effect.none )

                Err e ->
                    ( Failure e, Effect.none )

        GetRoomData roomResult ->
            case roomResult of
                Ok roomData ->
                    case model of
                        Success data ->
                            ( Success { room = roomData, game = data.game }, Effect.none )

                        _ ->
                            ( Success { room = roomData, game = { phase = 0, turn = "", nowPosition = [], history = [] } }, Effect.none )

                Err e ->
                    ( Failure e, Effect.none )


debugConvert : Http.Error -> String
debugConvert _ =
    "Error"


posToText : Maybe Int -> Html Msg
posToText pos =
    case pos of
        Just p ->
            td [] [ text (fromInt p) ]

        Nothing ->
            td [] [ text "-" ]


ticketToText : Maybe Ticket -> Html Msg
ticketToText ticket =
    case ticket of
        Just t ->
            td [] [ text (ticketToString t) ]

        Nothing ->
            td [] [ text "-" ]


ticketToString : Ticket -> String
ticketToString ticket =
    case ticket of
        Taxi ->
            "taxi"

        Bus ->
            "bus"

        Underground ->
            "ug"

        Secret ->
            "secret"


dec : Int -> List Player -> List Player -> List (Html Msg)
dec i a b =
    [ tr [] (td [] [ text (fromInt (i + 1)) ] :: List.map posToText (List.map .position a))
    , tr [] (td [] [ text "" ] :: List.map ticketToText (List.map .selectedTicket b))
    ]


decHelper : List (Html Msg) -> List (List (Html Msg)) -> List (Html Msg)
decHelper list1 list2 =
    concat (list2 ++ singleton list1)


nowPositionToText : List Player -> List (Html Msg)
nowPositionToText nowPosition =
    [ tr [ style "background-color" "#c9fcff", style "font-weight" "bold" ] (td [] [ text "NOW" ] :: List.map posToText (List.map .position nowPosition)) ]


phaseView : List Phase -> List Player -> List (Html Msg)
phaseView phaseList nowPosition =
    decHelper (nowPositionToText nowPosition) (List.map3 dec (List.map .phase phaseList) (List.map .player phaseList) (List.map .player phaseList))


playerHeaderView : Player -> Html Msg
playerHeaderView player =
    th [] [ text player.name ]


phaseHeaderView : Phase -> Html Msg
phaseHeaderView phase =
    tr []
        (th
            []
            [ text "" ]
            :: List.map
                playerHeaderView
                phase.player
        )


historyView : Model -> Html Msg
historyView model =
    case model of
        Loading ->
            div [] [ text "Loading..." ]

        Failure e ->
            div [] [ text (debugConvert (Debug.log "failure" e)) ]

        Success mainData ->
            div []
                [ h1 [] [ text mainData.room.roomName ]
                , p [ style "font-weight" "bold" ] [ text ("Now: Phase " ++ fromInt (mainData.game.phase + 1)) ]
                , table [ style "text-align" "center" ]
                    [ thead [] [ head (List.map phaseHeaderView mainData.game.history) |> Maybe.withDefault (tr [] []) ]
                    , tbody [] (phaseView (Debug.log "history" mainData.game.history) mainData.game.nowPosition)
                    ]
                ]


view : Model -> View Msg
view model =
    { title = "game"
    , body =
        div []
            [ historyView model
            , div [] [ a [ href "/" ] [ text "Go back to the home page" ] ]
            ]
    }



-- helper types


type alias GameData =
    -- TODO: add more fields
    { phase : Int
    , turn : String
    , nowPosition : List Player
    , history : List Phase
    }


type alias RoomData =
    { roomName : String
    }


type alias Phase =
    { phase : Int
    , player : List Player
    }


type alias Player =
    { name : String
    , position : Maybe Int
    , selectedTicket : Maybe Ticket
    }


type Ticket
    = Taxi
    | Bus
    | Underground
    | Secret



-- helper functions


fetchGameData : ( String, Maybe Int ) -> Cmd Msg
fetchGameData ( roomId, _ ) =
    Http.get
        { url = "/api/v1/game/" ++ roomId
        , expect = Http.expectJson GetGameData gameDataDecoder
        }


fetchRoomData : String -> Cmd Msg
fetchRoomData roomId =
    Http.get
        { url = "/api/v1/room/" ++ roomId
        , expect = Http.expectJson GetRoomData roomDataDecoder
        }


roomDataDecoder : Decoder RoomData
roomDataDecoder =
    map RoomData
        (field "roomName" string)


gameDataDecoder : Decoder GameData
gameDataDecoder =
    map4 GameData
        (field "phase" int)
        (field "turn" string)
        (field "nowPosition" (Decode.list playerDecoder))
        (field "history" (Decode.list phaseDecoder))


phaseDecoder : Decoder Phase
phaseDecoder =
    map2 Phase
        (field "phase" int)
        (field "player" (Decode.list playerDecoder))


playerDecoder : Decoder Player
playerDecoder =
    map3 Player
        (field "name" string)
        (maybe (field "position" int))
        (maybe (field "selectedTicket" ticketDecoder))


ticketDecoder : Decoder Ticket
ticketDecoder =
    string
        |> Decode.andThen
            (\ticket ->
                case ticket of
                    "TAXI" ->
                        Decode.succeed Taxi

                    "BUS" ->
                        Decode.succeed Bus

                    "UNDERGROUND" ->
                        Decode.succeed Underground

                    "SECRET" ->
                        Decode.succeed Secret

                    _ ->
                        Decode.fail "invalid ticket"
            )
