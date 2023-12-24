module Pages.Room.Id exposing (page)

import Debug
import Effect exposing (Effect)
import Html exposing (a, div, text)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map2, map3, maybe, string)
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
    = RenderPage (Result Http.Error GameData)


type Model
    = Failure Http.Error
    | Loading
    | Success GameData


init : ( String, Maybe Int ) -> ( Model, Effect Shared.Msg Msg )
init ( roomId, phase ) =
    ( Loading
    , Effect.fromCmd (fetchGameData ( roomId, phase ))
      -- TODO: fetch game data
    )


update : Msg -> Model -> ( Model, Effect Shared.Msg Msg )
update msg _ =
    case msg of
        RenderPage result ->
            case result of
                Ok gameData ->
                    ( Success gameData, Effect.none )

                Err e ->
                    ( Failure e, Effect.none )


debugConvert : Http.Error -> String
debugConvert _ =
    "error"


view : Model -> View Msg
view model =
    { title = "game"
    , body =
        div []
            [ text
                ("hello "
                    ++ (case model of
                            Loading ->
                                "loading"

                            Failure e ->
                                "failure" ++ debugConvert (Debug.log "httperror" e)

                            Success gameData ->
                                "success" ++ gameData.roomId ++ fromInt gameData.phase
                       )
                )
            , div [] [ a [ href "/" ] [ text "Go back to the home page" ] ]
            ]
    }



-- helper types


type alias GameData =
    -- TODO: add more fields
    { roomId : String
    , phase : Int
    , history : List Phase
    }


type alias Phase =
    { phase : Int
    , player : List Player
    }


type alias Player =
    { name : String
    , position : Int
    , selectedTicket : Maybe Ticket
    }


type Ticket
    = Taxi
    | Bus
    | Underground
    | Black



-- helper functions


fetchGameData : ( String, Maybe Int ) -> Cmd Msg
fetchGameData ( roomId, phase ) =
    Http.get
        { url = "/api/v1/room/" ++ roomId
        , expect = Http.expectJson RenderPage gameDataDecoder
        }


gameDataDecoder : Decoder GameData
gameDataDecoder =
    map3 GameData
        (field "roomId" string)
        (field "phase" int)
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
        (field "position" int)
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

                    "BLACK" ->
                        Decode.succeed Black

                    _ ->
                        Decode.fail "invalid ticket"
            )
