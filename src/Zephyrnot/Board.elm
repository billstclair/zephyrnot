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
    , Decoration(..)
    , Winner(..)
    , empty
    , get
    , render
    , set
    , winner
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)
import Set exposing (Set)
import Svg exposing (Svg, foreignObject, g, line, rect, svg)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , fill
        , fillOpacity
        , fontSize
        , height
        , r
        , rx
        , ry
        , stroke
        , strokeDasharray
        , strokeWidth
        , style
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
import Svg.Events as Events


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


lineWidthO2 : Int
lineWidthO2 =
    3


lineWidth : Int
lineWidth =
    lineWidthO2 * 2


type Decoration
    = NoDecoration
    | RowSelectedDecoration Int
    | ColSelectedDecoration Int
    | AlreadyFilledDecoration ( Int, Int )


render : Int -> (( Int, Int ) -> msg) -> Decoration -> Board -> Html msg
render size tagger decoration board =
    let
        sizeS =
            tos size

        delta =
            (size - lineWidth) // 6
    in
    svg
        [ width sizeS
        , height sizeS
        ]
    <|
        List.concat
            [ drawRows delta
            , drawCols delta tagger board
            , drawDecoration delta decoration
            ]


drawRows : Int -> List (Svg msg)
drawRows delta =
    List.map (drawRow delta) [ 0, 1, 2, 3, 4, 5 ]
        |> List.concat


fontSize : Int -> Int
fontSize delta =
    delta // 4


fontStyle : Int -> String
fontStyle fsize =
    "font-weight: bold; font-size: " ++ tos fsize


drawRow : Int -> Int -> List (Svg msg)
drawRow delta idx =
    let
        yc =
            delta * idx + delta // 2

        ycs =
            tos yc

        fsize =
            fontSize delta
    in
    [ Svg.line
        [ x1 <| tos (delta // 2)
        , y1 ycs
        , x2 <| tos (delta * 5 + delta // 2)
        , y2 ycs
        , strokeWidth <| tos lineWidth
        , stroke "black"
        ]
        []
    , Svg.text_
        [ x "0"
        , y <| tos (yc - 3 + fsize // 2)
        , style <| fontStyle fsize
        ]
        [ Svg.text <| rowToString idx ]
    ]


drawCols : Int -> (( Int, Int ) -> msg) -> Board -> List (Svg msg)
drawCols delta tagger board =
    List.map (drawCol delta tagger board) [ 0, 1, 2, 3, 4, 5 ]
        |> List.concat


drawCol : Int -> (( Int, Int ) -> msg) -> Board -> Int -> List (Svg msg)
drawCol delta tagger board idx =
    let
        xc =
            delta * idx + delta // 2

        xcs =
            tos xc

        fsize =
            fontSize delta
    in
    List.concat
        [ [ Svg.line
                [ x1 xcs
                , y1 <| tos (delta // 2)
                , x2 xcs
                , y2 <| tos (delta * 5 + delta // 2)
                , strokeWidth <| tos lineWidth
                , stroke "black"
                ]
                []
          , Svg.text_
                [ x xcs
                , y <| tos (delta * 6)
                , style <| fontStyle fsize
                , textAnchor "middle"
                ]
                [ Svg.text <| colToString idx ]
          ]
        , List.map (drawVertex delta idx tagger board) [ 0, 1, 2, 3, 4, 5 ]
            |> List.concat
        ]


connectWidth : Int -> Int
connectWidth delta =
    -- Ensures that the connector is at least as wide as the grid
    -- at all screen sizes.
    (delta + 48) // 12


drawVertex : Int -> Int -> (( Int, Int ) -> msg) -> Board -> Int -> List (Svg msg)
drawVertex delta colidx tagger board rowidx =
    let
        ( xc, yc ) =
            ( delta * colidx + delta // 2, delta * rowidx + delta // 2 )

        setp =
            get rowidx colidx board
    in
    [ [ Svg.circle
            [ cx <| tos xc
            , cy <| tos yc
            , r <| tos (delta // 6)
            , strokeWidth <| tos lineWidthO2
            , stroke "black"
            , fill
                (if setp then
                    "black"

                 else
                    "white"
                )
            ]
            []
      ]
    , if not setp then
        []

      else
        let
            lw =
                connectWidth delta

            leftp =
                get rowidx (colidx - 1) board

            rightp =
                get rowidx (colidx + 1) board

            upp =
                get (rowidx - 1) colidx board

            downp =
                get (rowidx + 1) colidx board
        in
        [ if not <| leftp || rightp then
            []

          else
            let
                left =
                    if leftp then
                        xc - delta // 2

                    else
                        xc

                right =
                    if rightp then
                        xc + delta // 2

                    else
                        xc
            in
            [ Svg.rect
                [ x <| tos left
                , y <| tos (yc - lw // 2)
                , width <| tos (right - left)
                , height <| tos lw
                , stroke "black"
                , fill "black"
                ]
                []
            ]
        , if not <| upp || downp then
            []

          else
            let
                top =
                    if upp then
                        yc - delta // 2

                    else
                        yc

                bottom =
                    if downp then
                        yc + delta // 2

                    else
                        yc
            in
            [ Svg.rect
                [ x <| tos (xc - lw // 2)
                , y <| tos top
                , width <| tos lw
                , height <| tos (bottom - top)
                , stroke "black"
                , fill "black"
                ]
                []
            ]
        ]
            |> List.concat
    , [ Svg.rect
            [ x <| tos (xc - delta // 2)
            , y <| tos (yc - delta // 2)
            , width <| tos delta
            , height <| tos delta
            , strokeWidth "0"
            , fillOpacity "0"
            , Events.onClick (tagger ( rowidx, colidx ))
            ]
            []
      ]
    ]
        |> List.concat


drawDecoration : Int -> Decoration -> List (Svg msg)
drawDecoration delta decoration =
    case decoration of
        NoDecoration ->
            []

        RowSelectedDecoration rowidx ->
            [ Svg.rect
                [ x <| tos (delta // 2 - delta // 6 - 1)
                , y <| tos (rowidx * delta + delta // 2 - delta // 6 - 1)
                , width <| tos (5 * delta + delta // 3 + 2)
                , height <| tos (delta // 3 + 2)
                , rx <| tos (delta // 6 + 1)
                , strokeWidth "0"
                , fillOpacity "0.3"
                ]
                []
            ]

        ColSelectedDecoration colidx ->
            [ Svg.rect
                [ y <| tos (delta // 2 - delta // 6 - 1)
                , x <| tos (colidx * delta + delta // 2 - delta // 6 - 1)
                , height <| tos (5 * delta + delta // 3 + 2)
                , width <| tos (delta // 3 + 2)
                , ry <| tos (delta // 6 + 1)
                , strokeWidth "0"
                , fillOpacity "0.3"
                ]
                []
            ]

        AlreadyFilledDecoration ( rowidx, colidx ) ->
            let
                ( xc, yc ) =
                    ( delta * colidx + delta // 2, delta * rowidx + delta // 2 )
            in
            [ Svg.circle
                [ cx <| tos xc
                , cy <| tos yc
                , r <| tos (delta // 4)
                , strokeWidth "0"
                , fill "red"
                ]
                []
            ]


rowNameDict : Dict Int String
rowNameDict =
    Dict.fromList
        [ ( 0, "F" )
        , ( 1, "E" )
        , ( 2, "D" )
        , ( 3, "C" )
        , ( 4, "B" )
        , ( 5, "A" )
        ]


rowToString : Int -> String
rowToString y =
    case Dict.get y rowNameDict of
        Nothing ->
            String.fromInt y

        Just s ->
            s


colToString : Int -> String
colToString x =
    tos <| x + 1
