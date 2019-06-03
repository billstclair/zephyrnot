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
    , Winner(..)
    , emptyPrivateGameState
    , messageToString
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


type Winner
    = NoWinner
    | HorizontalWinner
    | VerticalWinner


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


type alias SavedModel =
    { page : Page
    , decoration : Decoration
    , firstSelection : Decoration
    , chooseFirst : Player
    , player : Player
    , winner : Winner
    , path : List ( Int, Int )
    , moves : List String
    , board : Board
    , score : Score
    }



---
--- Talking to the server
---


type alias PrivateGameState =
    { receivedPlacement : Maybe Choice
    }


emptyPrivateGameState : PrivateGameState
emptyPrivateGameState =
    PrivateGameState Nothing


type alias GameState =
    { board : Board
    , moves : List String
    , players : PlayerNames
    , whoseTurn : Player
    , score : Score
    , winner : Winner

    -- not sent over the wire
    , private : PrivateGameState
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
    | JoinRsp
        { gameid : GameId
        , playerid : PlayerId
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
        , placement : Choice
        }
    | GameOverRsp
        { gameid : GameId
        , winner : Winner
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


messageToString : Message -> String
messageToString message =
    case message of
        NewReq _ ->
            "NewReq"

        NewRsp _ ->
            "NewRsp"

        JoinReq _ ->
            "JoinReq"

        JoinRsp _ ->
            "JoinRsp"

        LeaveReq _ ->
            "LeaveReq"

        LeaveRsp _ ->
            "LeaveRsp"

        UpdateReq _ ->
            "UpdateReq"

        UpdateRsp _ ->
            "UpdateRsp"

        PlayReq _ ->
            "PlayReq"

        PlayRsp _ ->
            "PlayRsp"

        GameOverRsp _ ->
            "GameOverRsp"

        ErrorRsp _ ->
            "ErrorRsp"

        ChatReq _ ->
            "ChatReq"

        ChatRsp _ ->
            "ChatRsp"
