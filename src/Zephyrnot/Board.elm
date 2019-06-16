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
    ( SizerKind(..)
    , colToString
    , count
    , empty
    , get
    , getSizer
    , render
    , rowToString
    , score
    , set
    , simulateGame
    , winner
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra as LE
import Random exposing (Seed)
import Set exposing (Set)
import Svg
    exposing
        ( Attribute
        , Svg
        , defs
        , foreignObject
        , g
        , line
        , marker
        , path
        , rect
        , svg
        )
import Svg.Attributes as Attributes
    exposing
        ( cx
        , cy
        , d
        , fill
        , fillOpacity
        , fontSize
        , height
        , markerEnd
        , markerHeight
        , markerStart
        , markerWidth
        , orient
        , points
        , r
        , refX
        , refY
        , rx
        , ry
        , stroke
        , strokeDasharray
        , strokeOpacity
        , strokeWidth
        , style
        , textAnchor
        , transform
        , viewBox
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


count : Board -> Int
count board =
    Array.toList board
        |> List.map Array.toList
        |> List.concat
        |> List.filter ((==) True)
        |> List.length


score : Board -> Int
score board =
    31 - count board


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
            ( ZephyrusWinner, hpath )

        else
            ( NotusWinner, vpath )

    else if hwin then
        ( ZephyrusWinner, hpath )

    else if vwin then
        ( NotusWinner, vpath )

    else
        ( NoWinner, [] )



---
--- Simulation
---


simulateGame : Seed -> ( Winner, Int, Seed )
simulateGame seed =
    let
        loop s b isH =
            let
                ( s2, b2 ) =
                    simulateMove s b isH

                player =
                    if isH then
                        Zephyrus

                    else
                        Notus

                ( win, _ ) =
                    winner player b2
            in
            if win == NoWinner then
                loop s2 b2 (not isH)

            else
                ( win, score b2, s2 )
    in
    loop seed empty True


simulateMove : Seed -> Board -> Bool -> ( Seed, Board )
simulateMove seed board isH =
    let
        gen =
            Random.int 0 5

        ( row, seed2 ) =
            Random.step gen seed

        ( col, seed3 ) =
            Random.step gen seed2

        ( board2, seed5 ) =
            if get row col board then
                if isH then
                    let
                        cnt =
                            emptyCols row board

                        ( c, seed4 ) =
                            Random.step (Random.int 0 (cnt - 1)) seed3
                    in
                    ( setEmptyCol row c board, seed4 )

                else
                    let
                        cnt =
                            emptyRows col board

                        ( r, seed4 ) =
                            Random.step (Random.int 0 (cnt - 1)) seed3
                    in
                    ( setEmptyRow r col board, seed4 )

            else
                ( set row col board, seed3 )
    in
    ( seed5, board2 )


emptyCols : Int -> Board -> Int
emptyCols row board =
    List.foldl
        (\col sum ->
            if get row col board then
                sum

            else
                sum + 1
        )
        0
        (List.range 0 5)


setEmptyCol : Int -> Int -> Board -> Board
setEmptyCol row col board =
    let
        loop cnt c =
            if not <| get row c board then
                if cnt == 0 then
                    set row c board

                else
                    loop (cnt - 1) (c + 1)

            else if c >= 5 then
                board

            else
                loop cnt (c + 1)
    in
    loop row 0


emptyRows : Int -> Board -> Int
emptyRows col board =
    List.foldl
        (\row sum ->
            if get row col board then
                sum

            else
                sum + 1
        )
        0
        (List.range 0 5)


setEmptyRow : Int -> Int -> Board -> Board
setEmptyRow row col board =
    let
        loop cnt r =
            if not <| get r col board then
                if cnt == 0 then
                    set r col board

                else
                    loop (cnt - 1) (r + 1)

            else if r >= 5 then
                board

            else
                loop cnt (r + 1)
    in
    loop row 0



---
--- Rendering
---


tos : Int -> String
tos x =
    String.fromInt x


lineWidthO2 : Int
lineWidthO2 =
    3


lineWidth : Int
lineWidth =
    lineWidthO2 * 2


connectSizer : Int -> ( Int, String )
connectSizer delta =
    ( connectWidth delta, connectColor )


pathSizer : Int -> ( Int, String )
pathSizer delta =
    ( pathWidth delta, pathColor )


type alias Sizer =
    { connect : Int -> ( Int, String )
    , path : Int -> ( Int, String )
    }


wideSizer : Sizer
wideSizer =
    Sizer connectSizer pathSizer


narrowSizer : Sizer
narrowSizer =
    { connect = \_ -> ( 2, "black" )
    , path = \_ -> ( 2, "orange" )
    }


type SizerKind
    = DefaultSizer
    | WideSizer


sizerKinds : List ( SizerKind, Sizer )
sizerKinds =
    [ ( DefaultSizer, narrowSizer )
    , ( WideSizer, wideSizer )
    ]


getSizer : SizerKind -> Sizer
getSizer kind =
    case LE.find (\( k, s ) -> k == kind) sizerKinds of
        Nothing ->
            narrowSizer

        Just ( _, s ) ->
            s


getConnectSizer : Maybe Sizer -> (Int -> ( Int, String ))
getConnectSizer sizer =
    (case sizer of
        Nothing ->
            getSizer DefaultSizer

        Just s ->
            s
    )
        |> .connect


getPathSizer : Maybe Sizer -> (Int -> ( Int, String ))
getPathSizer sizer =
    (case sizer of
        Nothing ->
            getSizer DefaultSizer

        Just s ->
            s
    )
        |> .path


render : Int -> (( Int, Int ) -> msg) -> Maybe Sizer -> Decoration -> Maybe Player -> Bool -> List ( Int, Int ) -> Board -> Html msg
render size tagger sizer decoration player rotated path board =
    let
        sizeS =
            tos size

        delta =
            (size - lineWidth) // 6

        translate =
            tos <| delta * 3
    in
    svg
        [ width sizeS
        , height sizeS
        ]
        [ g
            (if rotated then
                [ transform
                    ("rotate(-90,"
                        ++ translate
                        ++ ","
                        ++ translate
                        ++ ")"
                    )
                ]

             else
                []
            )
          <|
            List.concat
                [ [ defs []
                        [ arrowMarker ]
                  , drawCompass delta
                  ]
                , drawRows delta
                , drawCols delta sizer board
                , drawDirectionArrow delta player
                , drawPath delta sizer path
                , drawDecoration delta decoration
                , drawClickRects delta tagger
                ]
        ]


directionArrowColor : String
directionArrowColor =
    "green"


arrowMarker : Svg msg
arrowMarker =
    marker
        [ Attributes.id "arrow"
        , viewBox "0 0 10 10"
        , refX "5"
        , refY "5"
        , markerWidth "4"
        , markerHeight "4"
        , orient "auto-start-reverse"
        , stroke directionArrowColor
        , fill directionArrowColor
        ]
        [ path [ d "M 0 0 L 10 5 L 0 10 z" ]
            []
        ]


drawDirectionArrow : Int -> Maybe Player -> List (Svg msg)
drawDirectionArrow delta mplayer =
    case mplayer of
        Nothing ->
            []

        Just player ->
            let
                ( ( xx1, yy1 ), ( xx2, yy2 ) ) =
                    case player of
                        Zephyrus ->
                            ( ( delta, delta )
                            , ( delta * 5, delta )
                            )

                        Notus ->
                            ( ( delta, delta )
                            , ( delta, delta * 5 )
                            )
            in
            [ Svg.line
                [ x1 <| tos xx1
                , y1 <| tos yy1
                , x2 <| tos xx2
                , y2 <| tos yy2
                , strokeWidth <| tos lineWidth
                , stroke directionArrowColor
                , markerStart "url(#arrow)"
                , markerEnd "url(#arrow)"
                ]
                []
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


drawCols : Int -> Maybe Sizer -> Board -> List (Svg msg)
drawCols delta sizer board =
    List.map (drawCol delta sizer board) [ 0, 1, 2, 3, 4, 5 ]
        |> List.concat


drawCol : Int -> Maybe Sizer -> Board -> Int -> List (Svg msg)
drawCol delta sizer board idx =
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
        , List.map (drawVertex delta sizer idx board) [ 0, 1, 2, 3, 4, 5 ]
            |> List.concat
        ]



---
--- Parameters for sizing and coloring connections and the winning path.
---


connectWidth : Int -> Int
connectWidth delta =
    -- Ensures that the connector is at least as wide as the grid
    -- at all screen sizes.
    --2
    (delta + 48) // 12


pathWidth : Int -> Int
pathWidth delta =
    connectWidth delta - 4


connectColor : String
connectColor =
    --"white"
    "black"


pathColor : String
pathColor =
    "white"


centers : Int -> Int -> Int -> ( Int, Int )
centers delta rowidx colidx =
    ( delta * colidx + delta // 2, delta * rowidx + delta // 2 )


drawVertex : Int -> Maybe Sizer -> Int -> Board -> Int -> List (Svg msg)
drawVertex delta sizer colidx board rowidx =
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
        drawConnections delta rowidx colidx (getConnectSizer sizer) board
    ]
        |> List.concat


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


drawPath : Int -> Maybe Sizer -> List ( Int, Int ) -> List (Svg msg)
drawPath delta sizer path =
    let
        board =
            List.foldl (\( r, c ) b -> set r c b) empty path

        psizer =
            getPathSizer sizer
    in
    List.map (\( r, c ) -> drawConnections delta r c psizer board) path
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
                            -- Should really compute "1" here
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
