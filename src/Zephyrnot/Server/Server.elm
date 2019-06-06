port module Zephyrnot.Server.Server exposing (main)

import WebSocketFramework.Server
    exposing
        ( ServerMessageSender
        , UserFunctions
        , program
        , sendToOne
        , verbose
        )
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , Error
        , ErrorKind(..)
        , InputPort
        , OutputPort
        , ServerState
        )
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Interface as Interface
import Zephyrnot.Types as Types
    exposing
        ( GameState
        , Message(..)
        , Player
        )


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
    ( model, sendToOne ED.messageEncoder response outputPort socket )


userFunctions : UserFunctions ServerModel Message GameState Player
userFunctions =
    { encodeDecode = encodeDecode
    , messageProcessor = Interface.messageProcessor
    , messageSender = messageSender
    , messageToGameid = Nothing
    , messageToPlayerid = Nothing
    , autoDeleteGame = Nothing
    , gamesDeleter = Nothing
    , playersDeleter = Nothing
    , inputPort = inputPort
    , outputPort = outputPort
    }


main =
    program serverModel userFunctions Nothing



-- PORTS


port inputPort : InputPort msg


port outputPort : OutputPort msg
