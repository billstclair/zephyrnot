---------------------------------------------------------------------
--
-- Types.elm
-- Zephyrnot shared types.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Zephyrnot.Types exposing
    ( Board
    , Choice(..)
    , Decoration(..)
    , GameState
    , Message(..)
    , Page(..)
    , Player(..)
    , PlayerNames
    , PrivateGameState
    , SavedModel
    , Score
    , ServerState
    , Settings
    , Winner(..)
    , emptyPrivateGameState
    , emptySettings
    , messageToGameid
    , messageToPlayer
    , messageToPlayerid
    , otherPlayer
    , zeroScore
    )

import Array exposing (Array)
import WebSocketFramework.Types
    exposing
        ( GameId
        , PlayerId
        , ServerUrl
        )


type alias Board =
    Array (Array Bool)


type Player
    = Zephyrus -- Choose column
    | Notus -- Choose row


otherPlayer : Player -> Player
otherPlayer player =
    if player == Zephyrus then
        Notus

    else
        Zephyrus


type Winner
    = NoWinner
    | ZephyrusWinner
    | NotusWinner


type Decoration
    = NoDecoration
    | RowSelectedDecoration Int
    | ColSelectedDecoration Int
    | AlreadyFilledDecoration ( Int, Int )


type Page
    = MainPage
    | RulesPage
    | InstructionsPage
    | AuxPage


type alias Score =
    { zephyrusGames : Int
    , notusGames : Int
    , zephyrusScore : Int
    , notusScore : Int
    }


zeroScore : Score
zeroScore =
    Score 0 0 0 0


type alias Settings =
    { name : String
    , serverUrl : String
    , hideTitle : Bool
    }


emptySettings : Settings
emptySettings =
    { name = ""
    , serverUrl = "ws://localhost:8081"
    , hideTitle = False
    }


type alias SavedModel =
    { page : Page
    , decoration : Decoration
    , firstSelection : Decoration
    , chooseFirst : Player
    , player : Player
    , gameState : GameState
    , isLocal : Bool
    , isLive : Bool
    , gameid : String
    , playerid : String
    , settings : Settings
    }



---
--- Talking to the server
---


type alias PrivateGameState =
    { decoration : Decoration
    }


emptyPrivateGameState : PrivateGameState
emptyPrivateGameState =
    PrivateGameState NoDecoration


type alias GameState =
    { board : Board
    , moves : List String
    , players : PlayerNames
    , whoseTurn : Player
    , score : Score
    , winner : Winner
    , path : List ( Int, Int )
    , private : PrivateGameState --not sent over the wire
    }


type alias PlayerNames =
    { zephyrus : String
    , notus : String
    }


type Choice
    = ChooseRow Int
    | ChooseCol Int
    | ChooseResign Player
    | ChooseNew Player


type Message
    = NewReq
        { name : String
        , player : Player
        , isPublic : Bool
        , restoreState : Maybe GameState
        }
    | NewRsp
        { gameid : GameId
        , playerid : PlayerId
        , player : Player
        , name : String
        }
    | JoinReq
        { gameid : GameId
        , name : String
        }
    | ReJoinReq
        { gameid : GameId
        , playerid : PlayerId
        }
    | JoinRsp
        { gameid : GameId
        , playerid : Maybe PlayerId
        , player : Player
        , gameState : GameState
        }
    | LeaveReq { playerid : PlayerId }
    | LeaveRsp { gameid : GameId }
    | UpdateReq { playerid : PlayerId }
    | UpdateRsp
        { gameid : String
        , gameState : GameState
        }
      -- Game Play
    | PlayReq
        { playerid : PlayerId
        , placement : Choice
        }
    | PlayRsp
        { gameid : GameId
        , gameState : GameState
        , decoration : Decoration
        }
    | ResignRsp
        { gameid : GameId
        , gameState : GameState
        , player : Player
        }
    | AnotherGameRsp
        { gameid : GameId
        , gameState : GameState
        , player : Player
        }
    | GameOverRsp
        { gameid : GameId
        , gameState : GameState
        }
      -- Errors
    | ErrorRsp
        { request : String
        , text : String
        }
      -- Chat
    | ChatReq
        { playerid : String
        , text : String
        }
    | ChatRsp
        { gameid : String
        , name : String
        , text : String
        }


messageToPlayer : Message -> Maybe Player
messageToPlayer message =
    case message of
        NewReq { player } ->
            Just player

        NewRsp { player } ->
            Just player

        ResignRsp { player } ->
            Just player

        AnotherGameRsp { player } ->
            Just player

        _ ->
            Nothing


messageToPlayerid : Message -> Maybe PlayerId
messageToPlayerid message =
    case message of
        NewRsp { playerid } ->
            Just playerid

        JoinRsp { playerid } ->
            playerid

        LeaveReq { playerid } ->
            Just playerid

        UpdateReq { playerid } ->
            Just playerid

        PlayReq { playerid } ->
            Just playerid

        ChatReq { playerid } ->
            Just playerid

        _ ->
            Nothing


messageToGameid : Message -> Maybe GameId
messageToGameid message =
    case message of
        NewRsp { gameid } ->
            Just gameid

        JoinReq { gameid } ->
            Just gameid

        LeaveRsp { gameid } ->
            Just gameid

        UpdateRsp { gameid } ->
            Just gameid

        PlayRsp { gameid } ->
            Just gameid

        ResignRsp { gameid } ->
            Just gameid

        GameOverRsp { gameid } ->
            Just gameid

        ChatRsp { gameid } ->
            Just gameid

        _ ->
            Nothing


type alias ServerState =
    WebSocketFramework.Types.ServerState GameState Player
