---------------------------------------------------------------------
--
-- Board.elm
-- Zephyrnot board, storage and rendering.
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Zephyrnot.Board exposing
    ( Board
    , Winner(..)
    , empty
    , get
    , render
    , set
    , winner
    )

import Array exposing (Array)
import Html exposing (Html)
import Set exposing (Set)
import Svg exposing (Svg, foreignObject, g, line, rect, svg)
import Svg.Attributes
    exposing
        ( fill
        , fontSize
        , height
        , stroke
        , strokeDasharray
        , strokeWidth
        , textAnchor
        , transform
        , width
        , x
        , x1
        , x2
        , xlinkHref
        , y
        , y1
        , y2
        )


type alias Board =
    Array (Array Bool)


empty : Board
empty =
    Array.repeat 6 (Array.repeat 6 False)


get : Int -> Int -> Board -> Bool
get row col board =
    case Array.get row board of
        Nothing ->
            False

        Just r ->
            case Array.get col r of
                Nothing ->
                    False

                Just res ->
                    res


set : Int -> Int -> Board -> Board
set row col board =
    case Array.get row board of
        Nothing ->
            board

        Just r ->
            Array.set row
                (Array.set col True r)
                board


type Winner
    = NoWinner
    | HorizontalWinner
    | VerticalWinner


{-| It might be worthwhile to have an option to increment row or col first.
-}
winner : Board -> Winner
winner board =
    let
        findPath : Int -> Int -> (( Int, Int ) -> Bool) -> Set ( Int, Int ) -> ( Bool, Set ( Int, Int ) )
        findPath row col done seen =
            if done ( row, col ) then
                ( True, seen )

            else if row < 0 || row > 5 || col < 0 || col > 5 then
                ( False, seen )

            else if Set.member ( row, col ) seen then
                ( False, seen )

            else
                let
                    seen2 =
                        Set.insert ( row, col ) seen
                in
                if not <| get row col board then
                    ( False, seen2 )

                else
                    let
                        ( res3, seen3 ) =
                            findPath row (col + 1) done seen2
                    in
                    if res3 then
                        ( True, seen3 )

                    else
                        let
                            ( res4, seen4 ) =
                                findPath (row + 1) col done seen3
                        in
                        if res4 then
                            ( True, seen4 )

                        else
                            let
                                ( res5, seen5 ) =
                                    findPath row (col - 1) done seen4
                            in
                            if res5 then
                                ( True, seen5 )

                            else
                                findPath (row - 1) col done seen5

        ( hres, _ ) =
            findPath 0 0 (\( _, col ) -> col > 5) Set.empty
    in
    if hres then
        HorizontalWinner

    else
        let
            ( vres, _ ) =
                findPath 0 0 (\( row, _ ) -> row > 5) Set.empty
        in
        if vres then
            VerticalWinner

        else
            NoWinner


tos : Int -> String
tos x =
    String.fromInt x


overSixPtFive : Int -> Int
overSixPtFive x =
    2 * x // 13


lineWidth : String
lineWidth =
    "4"


render : Int -> (( Int, Int ) -> msg) -> Board -> Html msg
render size tagger board =
    let
        sizeS =
            tos size

        delta =
            overSixPtFive size
    in
    svg
        [ width sizeS
        , height sizeS
        ]
    <|
        List.concat
            [ drawRows delta
            , drawCols delta
            , drawBoard delta tagger board
            ]


drawRows : Int -> List (Svg msg)
drawRows delta =
    List.map (drawRow delta) [ 1, 2, 3, 4, 5, 6 ]


drawRow : Int -> Int -> Svg msg
drawRow delta idx =
    Svg.line
        [ x1 <| tos delta
        , y1 <| tos (delta * idx)
        , x2 <| tos (delta * 6)
        , y2 <| tos (delta * idx)
        , strokeWidth lineWidth
        , stroke "black"
        ]
        []


drawCols : Int -> List (Svg msg)
drawCols delta =
    List.map (drawCol delta) [ 1, 2, 3, 4, 5, 6 ]


drawCol : Int -> Int -> Svg msg
drawCol delta idx =
    Svg.line
        [ x1 <| tos (delta * idx)
        , y1 <| tos delta
        , x2 <| tos (delta * idx)
        , y2 <| tos (delta * 6)
        , strokeWidth lineWidth
        , stroke "black"
        ]
        []


drawBoard : Int -> (( Int, Int ) -> msg) -> Board -> List (Svg msg)
drawBoard delta tagger board =
    []
