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
    ( colToString
    , empty
    , get
    , render
    , rowToString
    , set
    , winner
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)
import Set exposing (Set)
import Svg exposing (Attribute, Svg, foreignObject, g, line, rect, svg)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , d
        , fill
        , fillOpacity
        , fontSize
        , height
        , points
        , r
        , rx
        , ry
        , stroke
        , strokeDasharray
        , strokeOpacity
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
import Zephyrnot.Types
    exposing
        ( Board
        , Decoration(..)
        , Player(..)
        , SavedModel
        , Winner(..)
        )


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


{-| It might be worthwhile to have an option to increment row or col first.
-}
winner : Player -> Board -> ( Winner, List ( Int, Int ) )
winner player board =
    let
        findPath : Int -> Int -> (( Int, Int ) -> Bool) -> Set ( Int, Int ) -> List ( Int, Int ) -> ( Bool, Set ( Int, Int ), List ( Int, Int ) )
        findPath row col done seen path =
            if done ( row, col ) then
                ( True, seen, path )

            else if row < 0 || row > 5 || col < 0 || col > 5 then
                ( False, seen, path )

            else if Set.member ( row, col ) seen then
                ( False, seen, path )

            else
                let
                    seen2 =
                        Set.insert ( row, col ) seen

                    path2 =
                        ( row, col ) :: path
                in
                if not <| get row col board then
                    ( False, seen2, path )

                else
                    let
                        ( res3, seen3, path3 ) =
                            findPath row (col + 1) done seen2 path2
                    in
                    if res3 then
                        ( True, seen3, path3 )

                    else
                        let
                            ( res4, seen4, path4 ) =
                                findPath (row + 1) col done seen3 path2
                        in
                        if res4 then
                            ( True, seen4, path4 )

                        else
                            let
                                ( res5, seen5, path5 ) =
                                    findPath row (col - 1) done seen4 path2
                            in
                            if res5 then
                                ( True, seen5, path5 )

                            else
                                findPath (row - 1) col done seen5 path2

        hloop row seen =
            let
                ( res, seen2, path2 ) =
                    findPath row 0 (\( _, col ) -> col > 5) seen []
            in
            if res then
                ( True, List.reverse path2 )

            else if row >= 5 then
                ( False, [] )

            else
                hloop (row + 1) seen2

        vloop col seen =
            let
                ( res, seen2, path2 ) =
                    findPath 0 col (\( row, _ ) -> row > 5) seen []
            in
            if res then
                ( True, List.reverse path2 )

            else if col >= 5 then
                ( False, [] )

            else
                vloop (col + 1) seen2
    in
    let
        ( hwin, hpath ) =
            hloop 0 Set.empty

        ( vwin, vpath ) =
            vloop 0 Set.empty
    in
    if hwin && vwin then
        if player == Zephyrus then
            ( HorizontalWinner, hpath )

        else
            ( VerticalWinner, vpath )

    else if hwin then
        ( HorizontalWinner, hpath )

    else if vwin then
        ( VerticalWinner, vpath )

    else
        ( NoWinner, [] )


tos : Int -> String
tos x =
    String.fromInt x


lineWidthO2 : Int
lineWidthO2 =
    3


lineWidth : Int
lineWidth =
    lineWidthO2 * 2


render : Int -> (( Int, Int ) -> msg) -> Decoration -> List ( Int, Int ) -> Board -> Html msg
render size tagger decoration path board =
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
            [ [ drawCompass delta ]
            , drawRows delta
            , drawCols delta board
            , drawPath delta path
            , drawDecoration delta decoration
            , drawClickRects delta tagger
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
    "font-weight: bold; font-size: " ++ tos fsize ++ ";"


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


drawClickRects : Int -> (( Int, Int ) -> msg) -> List (Svg msg)
drawClickRects delta tagger =
    let
        indices =
            [ 0, 1, 2, 3, 4, 5 ]

        docol rowidx colidx res =
            drawClickRect delta tagger rowidx colidx
                :: res

        dorow rowidx res =
            List.foldl (docol rowidx) res indices
    in
    List.foldl dorow [] indices


drawClickRect : Int -> (( Int, Int ) -> msg) -> Int -> Int -> Svg msg
drawClickRect delta tagger rowidx colidx =
    let
        xc =
            delta * colidx + delta // 2

        yc =
            delta * rowidx + delta // 2
    in
    Svg.rect
        [ x <| tos (xc - delta // 2)
        , y <| tos (yc - delta // 2)
        , width <| tos delta
        , height <| tos delta
        , strokeWidth "0"
        , fillOpacity "0"
        , Events.onClick (tagger ( rowidx, colidx ))
        ]
        []


drawCols : Int -> Board -> List (Svg msg)
drawCols delta board =
    List.map (drawCol delta board) [ 0, 1, 2, 3, 4, 5 ]
        |> List.concat


drawCol : Int -> Board -> Int -> List (Svg msg)
drawCol delta board idx =
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
        , List.map (drawVertex delta idx board) [ 0, 1, 2, 3, 4, 5 ]
            |> List.concat
        ]


connectWidth : Int -> Int
connectWidth delta =
    -- Ensures that the connector is at least as wide as the grid
    -- at all screen sizes.
    --2
    (delta + 48) // 12


connectColor : String
connectColor =
    --"white"
    "black"


centers : Int -> Int -> Int -> ( Int, Int )
centers delta rowidx colidx =
    ( delta * colidx + delta // 2, delta * rowidx + delta // 2 )


drawVertex : Int -> Int -> Board -> Int -> List (Svg msg)
drawVertex delta colidx board rowidx =
    let
        ( xc, yc ) =
            centers delta rowidx colidx

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
        drawConnections delta rowidx colidx connectSizer board
    ]
        |> List.concat


connectSizer : Int -> ( Int, String )
connectSizer delta =
    ( connectWidth delta, connectColor )


drawConnections : Int -> Int -> Int -> (Int -> ( Int, String )) -> Board -> List (Svg msg)
drawConnections delta rowidx colidx sizer board =
    let
        ( xc, yc ) =
            centers delta rowidx colidx

        ( lw, color ) =
            sizer delta

        leftp =
            get rowidx (colidx - 1) board

        rightp =
            get rowidx (colidx + 1) board

        upp =
            get (rowidx - 1) colidx board

        downp =
            get (rowidx + 1) colidx board

        lrOverlap =
            if upp || downp then
                lw // 2

            else
                0

        udOverlap =
            if leftp || rightp then
                lw // 2

            else
                0

        xyr =
            lw // 2
    in
    [ if not <| leftp || rightp then
        []

      else
        let
            left =
                if leftp then
                    xc - delta // 2 - lw // 2

                else
                    xc - lrOverlap

            right =
                if rightp then
                    xc + delta // 2 + lw // 2

                else
                    xc + lrOverlap
        in
        [ Svg.rect
            [ x <| tos left
            , y <| tos (yc - lw // 2)
            , width <| tos (right - left)
            , height <| tos lw
            , strokeWidth "0"
            , fill color
            , rx <| tos xyr
            ]
            []
        ]
    , if not <| upp || downp then
        []

      else
        let
            top =
                if upp then
                    yc - delta // 2 - lw // 2

                else
                    yc - udOverlap

            bottom =
                if downp then
                    yc + delta // 2 + lw // 2

                else
                    yc + udOverlap
        in
        [ Svg.rect
            [ x <| tos (xc - lw // 2)
            , y <| tos top
            , width <| tos lw
            , height <| tos (bottom - top)
            , strokeWidth "0"
            , fill color
            , ry <| tos xyr
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


pathSizer : Int -> ( Int, String )
pathSizer delta =
    ( connectWidth delta - 4, "white" )


drawPath : Int -> List ( Int, Int ) -> List (Svg msg)
drawPath delta path =
    let
        board =
            List.foldl (\( r, c ) b -> set r c b) empty path
    in
    List.map (\( r, c ) -> drawConnections delta r c pathSizer board) path
        |> List.concat


rowNameDict : Dict Int String
rowNameDict =
    Dict.fromList
        [ ( 0, "a" )
        , ( 1, "b" )
        , ( 2, "c" )
        , ( 3, "d" )
        , ( 4, "e" )
        , ( 5, "f" )
        ]


colToString : Int -> String
colToString y =
    case Dict.get y rowNameDict of
        Nothing ->
            String.fromInt y

        Just s ->
            s


rowToString : Int -> String
rowToString x =
    tos <| 6 - x


drawCompass : Int -> Svg msg
drawCompass delta =
    let
        c =
            3 * delta

        cs =
            tos c

        thickness =
            lineWidth + 4

        outerR =
            round (toFloat delta * sqrt (1 / 2))

        innerR =
            outerR - delta // 6 - lineWidthO2 // 2

        innerX =
            round (toFloat innerR / sqrt 2)

        shortR =
            innerR // 3

        shortX =
            round (toFloat shortR / sqrt 2)

        shorterX =
            2 * shortX // 3

        r =
            outerR

        longR =
            r + delta // 6

        fsize =
            fontSize delta

        fr =
            longR + fsize // 4

        connection =
            tos shortX ++ "," ++ tos shortX

        arcStart =
            "0," ++ tos r

        arcAngle =
            70

        arcRotate =
            (arcAngle - 90) // 2

        arcRadians =
            degrees arcAngle

        arcEnd =
            (tos <| round (toFloat r * sin arcRadians))
                ++ ","
                ++ (tos <| round (toFloat r * cos arcRadians))

        ellipseRadii =
            tos r ++ "," ++ tos r

        quarterImage =
            g
                []
                [ Svg.polygon
                    [ points <|
                        "0,0 "
                            ++ ("0," ++ tos longR ++ " ")
                            ++ (connection ++ " ")
                            ++ "0,0"
                    ]
                    []
                , Svg.polygon
                    [ points <|
                        (connection ++ " ")
                            ++ (tos innerX ++ "," ++ tos innerX ++ " ")
                            ++ (tos (shortX + shorterX) ++ "," ++ tos shortX ++ " ")
                            ++ connection
                    ]
                    []
                , g
                    [ transform <|
                        "rotate("
                            ++ tos arcRotate
                            ++ ")"
                    ]
                    [ Svg.path
                        [ d <|
                            ("M " ++ arcStart ++ " ")
                                ++ ("A"
                                        ++ ellipseRadii
                                        ++ " 0 0,0 "
                                        ++ arcEnd
                                   )
                        , strokeWidth <| tos thickness
                        , stroke "black"
                        , fill "none"
                        ]
                        []
                    ]
                ]

        compassText cx cy txt =
            Svg.text_
                [ x <| tos cx
                , y <| tos cy
                , style <| fontStyle fsize
                , textAnchor "middle"
                ]
                [ Svg.text txt ]
    in
    g
        [ transform <|
            "translate("
                ++ cs
                ++ " "
                ++ cs
                ++ ")"
        , fillOpacity "0.5"
        , strokeOpacity "0.5"
        ]
        [ quarterImage
        , g [ transform "rotate(90)" ] [ quarterImage ]
        , g [ transform "rotate(180)" ] [ quarterImage ]
        , g [ transform "rotate(270)" ] [ quarterImage ]
        , compassText 0 (negate fr) "N"
        , compassText 0 (fr + 3 * fsize // 4) "S"
        , compassText (negate <| fr + fsize // 3) (fsize // 3) "W"
        , compassText (fr + fsize // 3) (fsize // 3) "E"
        ]
