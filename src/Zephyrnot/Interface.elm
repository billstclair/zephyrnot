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
import WebSocketFramework.EncodeDecode as WFED
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
        , PublicType(..)
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
            { request = WFED.encodeMessage ED.messageEncoder message
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
        NewReq { name, player, publicType, restoreState } ->
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
                                { gs
                                    | players = players
                                }

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

                    forName =
                        case publicType of
                            PublicFor fn ->
                                Just fn

                            _ ->
                                Nothing

                    state6 =
                        if publicType == NotPublic then
                            state5

                        else
                            let
                                publicGame =
                                    { gameid = gameid
                                    , creator = name
                                    , player = player
                                    , forName = forName
                                    }
                                        |> ED.publicGameToFramework
                            in
                            { state5
                                | publicGames =
                                    ServerInterface.appendPublicGames
                                        publicGame
                                        state5.publicGames
                            }
                in
                ( state6
                , Just <|
                    NewRsp
                        { gameid = gameid
                        , playerid = playerid
                        , player = player
                        , name = name
                        , publicType = publicType
                        , gameState = gameState
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
                                    ( { players | zephyrus = name }, Zephyrus )

                                else
                                    ( { players | notus = name }, Notus )

                            ( playerid, state2 ) =
                                ServerInterface.newPlayerid state

                            state3 =
                                ServerInterface.addPlayer playerid
                                    { gameid = gameid
                                    , player = player
                                    }
                                    state2

                            gameState2 =
                                { gameState | players = players2 }

                            state4 =
                                ServerInterface.updateGame gameid
                                    gameState2
                                    state3
                        in
                        ( state4
                        , Just <|
                            JoinRsp
                                { gameid = gameid
                                , playerid = Just playerid
                                , player = player
                                , gameState = gameState2
                                }
                        )

        LeaveReq { playerid } ->
            case ServerInterface.getPlayer playerid state of
                Nothing ->
                    errorRes message state "Unknown playerid"

                Just { gameid, player } ->
                    let
                        state2 =
                            ServerInterface.removeGame gameid state
                    in
                    ( state2
                    , Just <|
                        LeaveRsp
                            { gameid = gameid
                            , player = player
                            }
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
                                            emptyGameState gameState.players

                                        gs2 =
                                            { gs | score = gameState.score }
                                    in
                                    ( ServerInterface.updateGame gameid gs2 state
                                    , Just <|
                                        AnotherGameRsp
                                            { gameid = gameid
                                            , gameState = gs2
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
                                                    NotusWinner

                                                Notus ->
                                                    ZephyrusWinner

                                        gs =
                                            { gameState | winner = winner }
                                                |> updateScore
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
                            if player == Zephyrus then
                                errorRes message
                                    state
                                    "Zephyrus may not choose rows"

                            else
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
                                                AlreadyFilledDecoration ( row, col )
                                in
                                doPlay decoration gameid gameState state

                        ChooseCol col ->
                            if player == Notus then
                                errorRes message
                                    state
                                    "Notus may not choose columns"

                            else
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
                                                AlreadyFilledDecoration ( row, col )
                                in
                                doPlay decoration gameid gameState state

        PublicGamesReq { subscribe, forName } ->
            -- subscribe is processed by the server code only
            let
                games =
                    state.publicGames
                        |> List.filterMap ED.frameworkToPublicGame
                        |> List.filter
                            (\game ->
                                (game.forName == Nothing)
                                    || (game.forName == forName)
                            )
            in
            ( state, Just <| PublicGamesRsp { games = games } )

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


doPlay : Decoration -> GameId -> GameState -> ServerState -> ( ServerState, Maybe Message )
doPlay decoration gameid gameState state =
    let
        whoseTurn =
            gameState.whoseTurn

        private =
            gameState.private

        board =
            gameState.board

        moves =
            gameState.moves

        ( ( newDecoration, newBoard, newWhoseTurn ), ( newWinner, newPath, newMoves ) ) =
            case decoration of
                AlreadyFilledDecoration ( row, col ) ->
                    if Board.get row col board then
                        ( ( decoration, board, whoseTurn ), ( NoWinner, [], moves ) )

                    else
                        let
                            board2 =
                                Board.set row col board

                            moves2 =
                                List.concat [ moves, [ cellName ( row, col ) ] ]

                            ( winner, path ) =
                                Board.winner whoseTurn board2

                            whoseTurn2 =
                                case winner of
                                    NoWinner ->
                                        Types.otherPlayer whoseTurn

                                    _ ->
                                        whoseTurn
                        in
                        ( ( NoDecoration, board2, whoseTurn2 )
                        , ( winner, path, moves2 )
                        )

                _ ->
                    ( ( decoration, board, whoseTurn ), ( NoWinner, [], moves ) )

        gs =
            { gameState
                | board = newBoard
                , moves = newMoves
                , whoseTurn = newWhoseTurn
                , winner = newWinner
                , path = newPath
                , private = { private | decoration = newDecoration }
            }
                |> updateScore

        state2 =
            ServerInterface.updateGame gameid gs state
    in
    case newWinner of
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


updateScore : GameState -> GameState
updateScore gameState =
    let
        score =
            gameState.score
    in
    case gameState.winner of
        NoWinner ->
            gameState

        ZephyrusWinner ->
            { gameState
                | score =
                    { score
                        | zephyrusGames =
                            score.zephyrusGames + 1
                        , zephyrusScore =
                            score.zephyrusScore + Board.score gameState.board
                    }
            }

        NotusWinner ->
            { gameState
                | score =
                    { score
                        | notusGames =
                            score.notusGames + 1
                        , notusScore =
                            score.notusScore + Board.score gameState.board
                    }
            }
