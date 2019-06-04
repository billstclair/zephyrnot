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


module Zephyrnot.Interface exposing (emptyGameState, messageProcessor)

import Debug
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , Plist
        , ReqRsp(..)
        , ServerState
        )
import Zephyrnot.Board as Board
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Types as Types
    exposing
        ( Board
        , Choice(..)
        , Decoration(..)
        , GameState
        , Message(..)
        , Player(..)
        , PlayerNames
        , Score
        , ServerState
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
    , path = []
    , private = Types.emptyPrivateGameState
    }


errorRes : Message -> ServerState -> String -> ( ServerState, Maybe Message )
errorRes message state text =
    ( state
    , Just <|
        ErrorRsp
            { request = Types.messageToString message
            , text = text
            }
    )


lookupGame : Message -> PlayerId -> ServerState -> Result ( ServerState, Maybe Message ) ( GameId, GameState, Player )
lookupGame message playerid state =
    let
        err text =
            Err <| errorRes message state text
    in
    case ServerInterface.getPlayer playerid state of
        Nothing ->
            err "Unknown playerid"

        Just { gameid, player } ->
            case ServerInterface.getGame gameid state of
                Nothing ->
                    err "Unknown gameid"

                Just gameState ->
                    Ok ( gameid, gameState, player )


messageProcessor : ServerState -> Message -> ( ServerState, Maybe Message )
messageProcessor state message =
    let
        foo =
            ( state, Nothing )
    in
    case message of
        NewReq { name, player, isPublic, restoreState } ->
            if name == "" then
                errorRes message state "Blank name not allowed."

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
                    errorRes message state "Unknown gameid"

                Just gameState ->
                    let
                        players =
                            gameState.players

                        { zephyrus, notus } =
                            players
                    in
                    if zephyrus /= "" && notus /= "" then
                        errorRes message state "Game already has two players"

                    else if name == "" || name == zephyrus || name == notus then
                        errorRes message
                            state
                            ("Blank or existing name: " ++ name)

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
                    errorRes message state "Unknown playerid"

                Just { gameid } ->
                    let
                        state2 =
                            ServerInterface.removeGame gameid state
                    in
                    ( state2
                    , Just <| LeaveRsp { gameid = gameid }
                    )

        UpdateReq { playerid } ->
            case lookupGame message playerid state of
                Err res ->
                    res

                Ok ( gameid, gameState, player ) ->
                    ( state
                    , Just <|
                        UpdateRsp
                            { gameid = gameid
                            , gameState = gameState
                            }
                    )

        PlayReq { playerid, placement } ->
            case lookupGame message playerid state of
                Err res ->
                    res

                Ok ( gameid, gameState, player ) ->
                    case placement of
                        ChooseNew _ ->
                            case gameState.winner of
                                NoWinner ->
                                    errorRes message state "Game not over"

                                _ ->
                                    let
                                        gs =
                                            emptyGameState <| gameState.players
                                    in
                                    ( ServerInterface.updateGame gameid gs state
                                    , Just <|
                                        AnotherGameRsp
                                            { gameid = gameid
                                            , gameState = gs
                                            , player = player
                                            }
                                    )

                        ChooseResign _ ->
                            case gameState.winner of
                                NoWinner ->
                                    let
                                        winner =
                                            case player of
                                                Zephyrus ->
                                                    VerticalWinner

                                                Notus ->
                                                    HorizontalWinner

                                        gs =
                                            { gameState | winner = winner }
                                    in
                                    ( ServerInterface.updateGame gameid gs state
                                    , Just <|
                                        ResignRsp
                                            { gameid = gameid
                                            , gameState = gs
                                            , player = player
                                            }
                                    )

                                _ ->
                                    errorRes message state "Game already over"

                        ChooseRow row ->
                            let
                                private =
                                    gameState.private

                                board =
                                    gameState.board

                                decoration =
                                    case private.decoration of
                                        NoDecoration ->
                                            RowSelectedDecoration row

                                        RowSelectedDecoration _ ->
                                            RowSelectedDecoration row

                                        ColSelectedDecoration col ->
                                            AlreadyFilledDecoration ( row, col )

                                        AlreadyFilledDecoration ( _, col ) ->
                                            if gameState.whoseTurn == Notus then
                                                AlreadyFilledDecoration ( row, col )

                                            else
                                                -- Not your turn, may not resolve
                                                private.decoration
                            in
                            doPlay decoration player gameid gameState state

                        ChooseCol col ->
                            let
                                private =
                                    gameState.private

                                board =
                                    gameState.board

                                decoration =
                                    case private.decoration of
                                        NoDecoration ->
                                            ColSelectedDecoration col

                                        ColSelectedDecoration _ ->
                                            ColSelectedDecoration col

                                        RowSelectedDecoration row ->
                                            AlreadyFilledDecoration ( row, col )

                                        AlreadyFilledDecoration ( row, _ ) ->
                                            if gameState.whoseTurn == Zephyrus then
                                                AlreadyFilledDecoration ( row, col )

                                            else
                                                -- Not your turn, may not resolve
                                                private.decoration
                            in
                            doPlay decoration player gameid gameState state

        ChatReq { playerid, text } ->
            case lookupGame message playerid state of
                Err res ->
                    res

                Ok ( gameid, gameState, player ) ->
                    let
                        players =
                            gameState.players

                        name =
                            case player of
                                Zephyrus ->
                                    players.zephyrus

                                Notus ->
                                    players.notus
                    in
                    ( state
                    , Just <|
                        ChatRsp
                            { gameid = gameid
                            , name = name
                            , text = text
                            }
                    )

        _ ->
            errorRes message state "Received a non-request."


doPlay : Decoration -> Player -> GameId -> GameState -> ServerState -> ( ServerState, Maybe Message )
doPlay decoration player gameid gameState state =
    let
        private =
            gameState.private

        board =
            gameState.board

        moves =
            gameState.moves

        ( ( newDecoration, newBoard, newPlayer ), ( winner, path, newMoves ) ) =
            case decoration of
                AlreadyFilledDecoration ( row, col ) ->
                    if Board.get row col board then
                        ( ( decoration, board, player ), ( NoWinner, [], moves ) )

                    else
                        let
                            board2 =
                                Board.set row col board

                            moves2 =
                                List.concat [ moves, [ cellName ( row, col ) ] ]

                            ( winner2, path2 ) =
                                Board.winner player board

                            player2 =
                                case winner2 of
                                    NoWinner ->
                                        Types.otherPlayer player

                                    _ ->
                                        player
                        in
                        ( ( NoDecoration, board2, player2 )
                        , ( winner2, path2, moves2 )
                        )

                _ ->
                    ( ( decoration, board, player ), ( NoWinner, [], moves ) )

        gs =
            { gameState
                | board = newBoard
                , moves = newMoves
                , whoseTurn = newPlayer
                , winner = winner
                , path = path
                , private = { private | decoration = newDecoration }
            }

        state2 =
            ServerInterface.updateGame gameid gs state
    in
    case winner of
        NoWinner ->
            ( state2
            , Just <|
                PlayRsp
                    { gameid = gameid
                    , gameState = gs
                    , decoration = newDecoration
                    }
            )

        _ ->
            ( state2
            , Just <|
                GameOverRsp
                    { gameid = gameid
                    , gameState = gs
                    }
            )


cellName : ( Int, Int ) -> String
cellName ( rowidx, colidx ) =
    Board.colToString colidx ++ Board.rowToString rowidx
