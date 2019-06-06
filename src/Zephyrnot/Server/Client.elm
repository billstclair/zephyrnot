---------------------------------------------------------------------
--
-- Client.elm
-- Simple low-level client for WebSocket server
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Client exposing (Model, Msg(..), br, init, is13, main, messageView, onEnter, subscriptions, update, view)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Task
import WebSocketFramework.EncodeDecode as WSFED
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Types as Types exposing (Choice(..), Message(..), Player(..))


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { messages : List String
    , input : String
    , server : String
    , error : Maybe String
    , state : State
    }


init : String -> ( Model, Cmd Msg )
init server =
    ( { messages = []
      , input = ""
      , server = server
      , error = Nothing
      , state = PortFunnels.initialState "unusedLocalStoragePrefix"
      }
    , openSocket server
    )


openSocket : String -> Cmd Msg
openSocket server =
    WebSocket.makeOpen (Debug.log "openSocket" server)
        |> WebSocket.send cmdPort



-- UPDATE


type Msg
    = InputMessage String
    | SubmitMessage
    | ServerMessage String
    | Process Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputMessage value ->
            ( { model | input = value }
            , Cmd.none
            )

        SubmitMessage ->
            ( { model
                | input = ""
                , error = Nothing
              }
            , WebSocket.makeSend model.server model.input
                |> WebSocket.send cmdPort
            )

        ServerMessage message ->
            ( { model
                | messages = message :: model.messages
              }
            , Cmd.none
            )

        Process value ->
            case
                PortFunnels.processValue funnelDict value model.state model
            of
                Err error ->
                    ( { model | error = Just error }
                    , Cmd.none
                    )

                Ok res ->
                    res



-- WebSocket interface


handlers : List (Handler Model Msg)
handlers =
    [ WebSocketHandler socketHandler
    ]


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict handlers getCmdPort


{-| Get a possibly simulated output port.
-}
getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort Process moduleName False


{-| The real output port.
-}
cmdPort : Value -> Cmd Msg
cmdPort =
    PortFunnels.getCmdPort Process "" False


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | state = state }
    in
    case response of
        WebSocket.MessageReceivedResponse { message } ->
            ( model, Task.perform ServerMessage <| Task.succeed message )

        ErrorResponse error ->
            ( { model | error = Just <| WebSocket.errorToString error }
            , case error of
                WebSocket.SocketNotOpenError _ ->
                    openSocket model.server

                _ ->
                    Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    PortFunnels.subscriptions Process



-- VIEW


onEnter : Msg -> H.Attribute Msg
onEnter message =
    E.on "keydown"
        (E.keyCode |> Decode.andThen (is13 message))


is13 : a -> Int -> Decoder a
is13 a code =
    if code == 13 then
        Decode.succeed a

    else
        Decode.fail "not the right key code"


messageView : String -> Html Msg
messageView message =
    H.li
        []
        [ H.text message ]


br : Html Msg
br =
    H.br [] []


exampleMessages : List Message
exampleMessages =
    [ NewReq
        { name = "Bill"
        , player = Zephyrus
        , isPublic = False
        , restoreState = Nothing
        }
    , JoinReq
        { gameid = "GAMEID"
        , name = "Chris"
        }
    , PlayReq
        { playerid = "PLAYERID"
        , placement = ChooseRow 0
        }
    , PlayReq
        { playerid = "PLAYERID"
        , placement = ChooseCol 0
        }
    , PlayReq
        { playerid = "PLAYERID"
        , placement = ChooseResign Zephyrus
        }
    , PlayReq
        { playerid = "PLAYERID"
        , placement = ChooseNew Zephyrus
        }
    , LeaveReq { playerid = "PLAYERID" }
    , UpdateReq { playerid = "PLAYERID" }
    , ChatReq
        { playerid = "PLAYERID"
        , text = "Hello, World!"
        }
    ]


exampleMessageStrings : List String
exampleMessageStrings =
    exampleMessages
        |> List.map (WSFED.encodeMessage ED.messageEncoder)


view : Model -> Html Msg
view model =
    H.div
        []
        [ H.ul [] (List.map messageView model.messages)
        , H.input
            [ A.type_ "text"
            , A.placeholder "Message..."
            , A.value model.input
            , A.size 50
            , E.onInput InputMessage
            , onEnter SubmitMessage
            ]
            []
        , H.p []
            [ H.text "Examples:"
            , H.span [] <|
                List.map (\s -> H.span [] [ br, H.text s ])
                    exampleMessageStrings
            ]
        ]