port module Zephyrnot.Server.Server exposing (main)

import WebSocketFramework.Server
    exposing
        ( Msg
        , ServerMessageSender
        , Socket
        , UserFunctions
        , program
        , verbose
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , Error
        , ErrorKind(..)
        , GameId
        , InputPort
        , OutputPort
        )
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Interface as Interface
import Zephyrnot.Types as Types
    exposing
        ( Decoration(..)
        , GameState
        , Message(..)
        , Player
        )


type alias Model =
    WebSocketFramework.Server.Model ServerModel Message GameState Player


type alias ServerState =
    WebSocketFramework.Types.ServerState GameState Player


type alias ServerModel =
    ()


serverModel : ServerModel
serverModel =
    ()


tos : Int -> String
tos x =
    String.fromInt x


errorWrapper : Error Message -> Message
errorWrapper { kind, description, message } =
    case kind of
        JsonParseError ->
            let
                err =
                    case message of
                        Err msg ->
                            msg

                        Ok msg ->
                            Debug.toString msg
            in
            ErrorRsp
                { request = description
                , text = "JSON parser error: " ++ err
                }

        _ ->
            ErrorRsp
                { request = ""
                , text = Debug.toString message
                }


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = ED.messageEncoder
    , decoder = ED.messageDecoder
    , errorWrapper = Just errorWrapper
    }


messageSender : ServerMessageSender ServerModel Message GameState Player
messageSender model socket state request response =
    let
        sender =
            case request of
                NewReq _ ->
                    sendToOne

                UpdateReq _ ->
                    sendToOne

                _ ->
                    case response of
                        JoinRsp _ ->
                            sendJoinRsp model

                        PlayRsp _ ->
                            sendPlayRsp model

                        _ ->
                            sendToAll model
    in
    ( model, sender response socket )


sendToOne : Message -> Socket -> Cmd Msg
sendToOne response socket =
    WebSocketFramework.Server.sendToOne ED.messageEncoder response outputPort socket


sendToAll : Model -> Message -> Socket -> Cmd Msg
sendToAll model response socket =
    case Types.messageToGameid response of
        Nothing ->
            sendToOne response socket

        Just gameid ->
            WebSocketFramework.Server.sendToAll gameid
                model
                ED.messageEncoder
                response


sendToOthers : Model -> Message -> Socket -> Cmd Msg
sendToOthers model response socket =
    case Types.messageToGameid response of
        Nothing ->
            Cmd.none

        Just gameid ->
            WebSocketFramework.Server.sendToOthers gameid
                socket
                model
                ED.messageEncoder
                response


sendJoinRsp : Model -> Message -> Socket -> Cmd Msg
sendJoinRsp model response socket =
    case response of
        JoinRsp record ->
            Cmd.batch
                [ sendToOne response socket
                , sendToOthers model
                    (JoinRsp { record | playerid = Nothing })
                    socket
                ]

        _ ->
            sendToAll model response socket


sendPlayRsp : Model -> Message -> Socket -> Cmd Msg
sendPlayRsp model response socket =
    case response of
        PlayRsp { decoration } ->
            let
                toOne =
                    case decoration of
                        RowSelectedDecoration _ ->
                            True

                        ColSelectedDecoration _ ->
                            True

                        _ ->
                            False
            in
            if toOne then
                sendToOne response socket

            else
                sendToAll model response socket

        _ ->
            sendToAll model response socket


userFunctions : UserFunctions ServerModel Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = Interface.messageProcessor
    , messageSender = messageSender
    , messageToGameid = Just Types.messageToGameid
    , messageToPlayerid = Just Types.messageToPlayerid
    , autoDeleteGame = Nothing
    , gamesDeleter = Nothing
    , playersDeleter = Nothing
    , inputPort = inputPort
    , outputPort = outputPort
    }


{-| Debugging version
-}
messageProcessor : ServerState -> Message -> ( ServerState, Maybe Message )
messageProcessor state message =
    Interface.messageProcessor (Debug.log "messageProcessor" state)
        (Debug.log "  message" message)
        |> Debug.log "  output"


main =
    program serverModel userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
