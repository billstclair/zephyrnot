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
    , SavedModel
    , Score
    , Winner(..)
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


type alias GameState =
    { board : Board
    , moves : List String
    , players : PlayerNames
    , whoseturn : Player
    , score : Score
    , winner : Winner
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
        , isPublic : Bool
        , restoreState : Maybe GameState
        }
    | NewRsp
        { gameid : GameId
        , playerid : PlayerId
        , name : String
        }
    | JoinReq
        { gameid : GameId
        , name : String
        }
    | JoinRsp
        { playerid : PlayerId
        , names : PlayerNames
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
    | GameOverRsp { winner : Winner }
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
        , player : Player
        , text : String
        }
