---------------------------------------------------------------------
--
-- Interface.elm
-- Zephyrnot server interface.
-- Runs on local machine for local play, and server for networked play.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Zephyrnot.Interface exposing (messageProcessor)

import Debug
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types exposing (Plist, ReqRsp(..), ServerState)
import Zephyrnot.Board as Board
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Types as Types
    exposing
        ( Board
        , Choice(..)
        , GameState
        , Message(..)
        , Player(..)
        , PlayerNames
        , Score
        , Winner(..)
        )


emptyGameState : PlayerNames -> GameState
emptyGameState players =
    { board = Board.empty
    , moves = []
    , players = players
    , whoseTurn = Zephyrus
    , score = Types.zeroScore
    , winner = NoWinner
    , private = Types.emptyPrivateGameState
    }


errorRsp : String -> String -> Message
errorRsp request text =
    ErrorRsp
        { request = request
        , text = text
        }


messageProcessor : ServerState GameState Player -> Message -> ( ServerState GameState Player, Maybe Message )
messageProcessor state message =
    let
        foo =
            ( state, Nothing )
    in
    case message of
        NewReq { name, player, isPublic, restoreState } ->
            if name == "" then
                ( state
                , Just <|
                    errorRsp "NewReq" "Blank name not allowed."
                )

            else
                let
                    players =
                        case player of
                            Zephyrus ->
                                { zephyrus = name
                                , notus = ""
                                }

                            Notus ->
                                { zephyrus = ""
                                , notus = name
                                }

                    gameState =
                        case restoreState of
                            Nothing ->
                                emptyGameState players

                            Just gs ->
                                { gs | players = players }

                    ( gameid, state2 ) =
                        ServerInterface.newGameid state

                    ( playerid, state3 ) =
                        ServerInterface.newPlayerid state2

                    playerInfo =
                        { gameid = gameid, player = player }

                    state4 =
                        ServerInterface.addGame gameid gameState state3

                    state5 =
                        ServerInterface.addPlayer playerid playerInfo state4
                in
                ( state5
                , Just <|
                    NewRsp
                        { gameid = gameid
                        , playerid = playerid
                        , player = player
                        , name = name
                        }
                )

        JoinReq { gameid, name } ->
            case ServerInterface.getGame gameid state of
                Nothing ->
                    ( state
                    , Just <|
                        errorRsp "JoinReq" "Unknown gameid"
                    )

                Just gameState ->
                    let
                        players =
                            gameState.players

                        { zephyrus, notus } =
                            players
                    in
                    if zephyrus /= "" && notus /= "" then
                        ( state
                        , Just <|
                            errorRsp "JoinReq" "Game already has two players"
                        )

                    else if name == "" || name == zephyrus || name == notus then
                        ( state
                        , Just <|
                            errorRsp "JoinReq"
                                ("Blank or existing name: " ++ name)
                        )

                    else
                        let
                            ( players2, player ) =
                                if zephyrus == "" then
                                    ( { players | notus = name }, Notus )

                                else
                                    ( { players | zephyrus = name }, Zephyrus )

                            ( playerid, state2 ) =
                                ServerInterface.newPlayerid state

                            gameState2 =
                                { gameState | players = players2 }

                            state3 =
                                ServerInterface.updateGame gameid
                                    gameState2
                                    state2
                        in
                        ( state3
                        , Just <|
                            JoinRsp
                                { gameid = gameid
                                , playerid = playerid
                                , player = player
                                , gameState = gameState2
                                }
                        )

        LeaveReq { playerid } ->
            case ServerInterface.getPlayer playerid state of
                Nothing ->
                    ( state, Just <| errorRsp "LeaveReq" "Unknown playerid" )

                Just { gameid } ->
                    let
                        state2 =
                            ServerInterface.removeGame gameid state
                    in
                    ( state2
                    , Just <| LeaveRsp { gameid = gameid }
                    )

        UpdateReq { playerid } ->
            foo

        PlayReq { playerid, placement } ->
            foo

        ChatReq { playerid, text } ->
            foo

        _ ->
            ( state
            , Just <|
                errorRsp (Types.messageToString message) "Received a non-request."
            )
