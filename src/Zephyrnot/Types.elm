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
    , Decoration(..)
    , Player(..)
    , SavedModel
    , Winner(..)
    )

import Array exposing (Array)


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


type alias SavedModel =
    { decoration : Decoration
    , chooseFirst : Player
    , player : Player
    , winner : Winner
    , path : List ( Int, Int )
    , moves : List String
    , board : Board
    }
