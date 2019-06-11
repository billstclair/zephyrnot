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
    , PublicGame
    , PublicType(..)
    , SavedModel
    , Score
    , ServerState
    , Settings
    , Socket
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
import Set exposing (Set)
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
    , hideTitle : Bool
    }


emptySettings : Settings
emptySettings =
    { name = ""
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


type alias Socket =
    String


type alias PrivateGameState =
    { decoration : Decoration
    , subscribers : Set Socket
    }


emptyPrivateGameState : PrivateGameState
emptyPrivateGameState =
    PrivateGameState NoDecoration Set.empty


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


type PublicType
    = NotPublic
    | EntirelyPublic
    | PublicFor String


type Message
    = NewReq
        { name : String
        , player : Player
        , publicType : PublicType
        , restoreState : Maybe GameState
        }
    | NewRsp
        { gameid : GameId
        , playerid : PlayerId
        , player : Player
        , name : String
        , publicType : PublicType
        , gameState : GameState
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
    | LeaveRsp { gameid : GameId, player : Player }
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
      -- Public games
    | PublicGamesReq
        { subscribe : Bool
        , forName : Maybe String
        }
    | PublicGamesRsp { games : List PublicGame }
    | PublicGamesUpdateRsp
        { added : List PublicGame
        , removed : List String
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


type alias PublicGame =
    { gameid : GameId
    , creator : String
    , player : Player
    , forName : Maybe String
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

        JoinRsp { gameid } ->
            Just gameid

        LeaveRsp { gameid } ->
            Just gameid

        UpdateRsp { gameid } ->
            Just gameid

        PlayRsp { gameid } ->
            Just gameid

        ResignRsp { gameid } ->
            Just gameid

        AnotherGameRsp { gameid } ->
            Just gameid

        GameOverRsp { gameid } ->
            Just gameid

        ChatRsp { gameid } ->
            Just gameid

        _ ->
            Nothing


type alias ServerState =
    WebSocketFramework.Types.ServerState GameState Player
