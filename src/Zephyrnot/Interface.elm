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

import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.Types exposing (Plist, ReqRsp(..), ServerState)
import Zephyrnot.Types
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


messageProcessor : ServerState GameState Player -> Message -> ( ServerState GameState Player, Maybe Message )
messageProcessor state message =
    let
        foo =
            ( state, Nothing )
    in
    case message of
        NewReq { name, isPublic, restoreState } ->
            foo

        JoinReq { gameid, name } ->
            foo

        LeaveReq { playerid } ->
            foo

        UpdateReq { playerid } ->
            foo

        PlayReq { playerid, placement } ->
            foo

        ChatReq { playerid, text } ->
            foo
