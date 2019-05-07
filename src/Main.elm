---------------------------------------------------------------------
--
-- Main.elm
-- Zephyrnot top-level
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
        , button
        , div
        , fieldset
        , h2
        , h3
        , h4
        , img
        , input
        , label
        , optgroup
        , option
        , p
        , select
        , span
        , table
        , td
        , text
        , textarea
        , tr
        )
import Html.Attributes as Attributes
    exposing
        ( align
        , alt
        , autofocus
        , checked
        , class
        , cols
        , colspan
        , disabled
        , height
        , href
        , id
        , name
        , placeholder
        , readonly
        , rows
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (on, onCheck, onClick, onInput)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
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
import Svg.Button as SB exposing (Button, Content(..))
import Svg.Events
import Task
import Url exposing (Url)
import Zephyrnot.Board as Board
    exposing
        ( Board
        , Decoration(..)
        , Player(..)
        , Winner(..)
        )


type alias Model =
    { key : Key
    , windowSize : ( Int, Int )
    , decoration : Decoration
    , chooseFirst : Player
    , player : Player
    , winner : Winner
    , moves : List String
    , board : Board
    }


type Msg
    = Noop
    | SetDecoration Decoration
    | SetChooseFirst Player
    | NewGame
    | Click ( Int, Int )
    | WindowResize Int Int
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }


{-| No longer used. An initial state to test Board.render
-}
initializeBoard : Board -> Board
initializeBoard board =
    board
        |> Board.set 0 3
        |> Board.set 1 1
        |> Board.set 1 2
        |> Board.set 2 2
        |> Board.set 3 2
        |> Board.set 3 1
        |> Board.set 3 3
        |> Board.set 3 4
        |> Board.set 2 4
        |> Board.set 4 4
        |> Board.set 4 5


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { key = key
    , windowSize = ( 1024, 768 )
    , decoration = NoDecoration
    , chooseFirst = Zephyrus
    , player = Zephyrus
    , winner = NoWinner
    , moves = []
    , board = Board.empty
    }
        |> withCmds
            [ Task.perform getViewport Dom.getViewport ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        SetDecoration decoration ->
            { model | decoration = decoration }
                |> withNoCmd

        SetChooseFirst player ->
            { model | chooseFirst = player }
                |> withNoCmd

        NewGame ->
            { model
                | board = Board.empty
                , decoration = NoDecoration
                , player = Zephyrus
                , winner = NoWinner
                , moves = []
            }
                |> withNoCmd

        Click ( row, col ) ->
            if model.winner /= NoWinner then
                model |> withNoCmd

            else
                let
                    mdl =
                        case model.decoration of
                            NoDecoration ->
                                { model
                                    | decoration =
                                        if model.chooseFirst == Zephyrus then
                                            ColSelectedDecoration col

                                        else
                                            RowSelectedDecoration row
                                }

                            ColSelectedDecoration c ->
                                let
                                    filled =
                                        Board.get row c model.board
                                in
                                { model
                                    | decoration =
                                        if filled then
                                            AlreadyFilledDecoration ( row, c )

                                        else
                                            NoDecoration
                                    , moves =
                                        if filled then
                                            model.moves

                                        else
                                            cellName ( row, c ) :: model.moves
                                    , board =
                                        if filled then
                                            model.board

                                        else
                                            Board.set row c model.board
                                    , player =
                                        if filled then
                                            model.player

                                        else
                                            otherPlayer model.player
                                }

                            RowSelectedDecoration r ->
                                let
                                    filled =
                                        Board.get r col model.board
                                in
                                { model
                                    | decoration =
                                        if filled then
                                            AlreadyFilledDecoration ( r, col )

                                        else
                                            NoDecoration
                                    , moves =
                                        if filled then
                                            model.moves

                                        else
                                            cellName ( r, col ) :: model.moves
                                    , board =
                                        if filled then
                                            model.board

                                        else
                                            Board.set r col model.board
                                    , player =
                                        if filled then
                                            model.player

                                        else
                                            otherPlayer model.player
                                }

                            AlreadyFilledDecoration ( r, c ) ->
                                let
                                    ( rr, cc ) =
                                        if model.player == Zephyrus then
                                            ( r, col )

                                        else
                                            ( row, c )

                                    filled =
                                        Board.get rr cc model.board
                                in
                                { model
                                    | decoration =
                                        if filled then
                                            AlreadyFilledDecoration ( rr, cc )

                                        else
                                            NoDecoration
                                    , moves =
                                        if filled then
                                            model.moves

                                        else
                                            (cellName ( r, c )
                                                ++ "/"
                                                ++ cellName ( rr, cc )
                                            )
                                                :: model.moves
                                    , board =
                                        if filled then
                                            model.board

                                        else
                                            Board.set rr cc model.board
                                    , player =
                                        if filled then
                                            model.player

                                        else
                                            otherPlayer model.player
                                }
                in
                { mdl | winner = Board.winner model.player mdl.board }
                    |> withNoCmd

        WindowResize w h ->
            { model | windowSize = ( w, h ) }
                |> withNoCmd

        HandleUrlRequest request ->
            ( model
            , case request of
                Internal url ->
                    -- For now
                    Navigation.load <| Url.toString url

                External urlString ->
                    Navigation.load urlString
            )

        HandleUrlChange url ->
            model |> withNoCmd


cellName : ( Int, Int ) -> String
cellName ( rowidx, colidx ) =
    Board.rowToString rowidx ++ Board.colToString colidx


otherPlayer : Player -> Player
otherPlayer player =
    if player == Zephyrus then
        Notus

    else
        Zephyrus


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResize
        ]


br : Html Msg
br =
    Html.br [] []


boardSize : Model -> Int
boardSize model =
    let
        ( w, h ) =
            model.windowSize
    in
    min (90 * w) (70 * h) // 100


view : Model -> Document Msg
view model =
    let
        message =
            case model.winner of
                HorizontalWinner ->
                    "Zephyrus wins!"

                VerticalWinner ->
                    "Notus wins!"

                NoWinner ->
                    case model.decoration of
                        NoDecoration ->
                            if model.chooseFirst == Zephyrus then
                                "Zephyrus pick a column"

                            else
                                "Notus pick a row"

                        ColSelectedDecoration _ ->
                            "Notus pick a row"

                        RowSelectedDecoration _ ->
                            "Zephyrus pick a column"

                        AlreadyFilledDecoration _ ->
                            if model.player == Zephyrus then
                                "Zephyrus pick another column"

                            else
                                "Notus pick another row"
    in
    { title = "Zephyrnot"
    , body =
        [ div
            [ align "center"
            ]
            [ h2 [ style "margin" "0" ]
                [ text "Zephyrnot" ]
            , h3 [ style "margin" "0" ]
                [ text "Feud of the Winds" ]
            , p [ style "margin" "0" ]
                [ text "Invented by Chris St. Clair" ]
            , p []
                [ Board.render (boardSize model)
                    Click
                    model.decoration
                    model.board
                ]
            , p
                []
                [ span
                    [ style "color"
                        (if model.winner == NoWinner then
                            "red"

                         else
                            "orange"
                        )
                    , style "font-weight"
                        (if model.winner == NoWinner then
                            "normal"

                         else
                            "bold"
                        )
                    , style "font-size"
                        (if model.winner == NoWinner then
                            "100%"

                         else
                            "120%"
                        )
                    ]
                    [ text message ]
                , br
                , if model.winner /= NoWinner then
                    text ""

                  else
                    span []
                        [ text "Stone Placer: "
                        , text <|
                            case model.player of
                                Zephyrus ->
                                    "Zephyrus"

                                Notus ->
                                    "Notus"
                        ]
                , br
                , text "Choose first: "
                , radio "choose"
                    "Zephyrus"
                    (model.chooseFirst == Zephyrus)
                    (SetChooseFirst Zephyrus)
                , text " "
                , radio "choose"
                    "Notus"
                    (model.chooseFirst == Notus)
                    (SetChooseFirst Notus)
                , br
                , button
                    [ onClick NewGame ]
                    [ text "New Game" ]
                ]
            , p []
                [ text "Moves: "
                , text <| movesToString (List.reverse model.moves)
                ]
            , p []
                [ a
                    [ href "Zephyrnot.pdf"
                    , target "_blank"
                    ]
                    [ text "Instructions" ]
                , br
                , a
                    [ href "https://github.com/billstclair/zephyrnot/"
                    , target "_blank"
                    ]
                    [ text "GitHub" ]
                ]
            ]
        ]
    }


pairup : List String -> List ( String, String )
pairup strings =
    let
        loop list res =
            case list of
                [] ->
                    List.reverse res

                [ x ] ->
                    List.reverse <| ( x, "" ) :: res

                x :: (y :: tail) ->
                    loop tail (( x, y ) :: res)
    in
    loop strings []


movesToString : List String -> String
movesToString moves =
    pairup moves
        |> List.map pairToString
        |> List.intersperse ", "
        |> String.concat


pairToString : ( String, String ) -> String
pairToString ( a, b ) =
    if b == "" then
        a

    else
        "(" ++ a ++ ", " ++ b ++ ")"



-- For testing. No longer used.


decorationRadios : Model -> Html Msg
decorationRadios model =
    let
        ( none, ( rowp, colp, filledp ) ) =
            case model.decoration of
                NoDecoration ->
                    ( True, ( False, False, False ) )

                RowSelectedDecoration _ ->
                    ( False, ( True, False, False ) )

                ColSelectedDecoration _ ->
                    ( False, ( False, True, False ) )

                _ ->
                    ( False, ( False, False, True ) )
    in
    p []
        [ radio "decoration" "none" none (SetDecoration NoDecoration)
        , text " "
        , radio "decoration" "row" rowp (SetDecoration (RowSelectedDecoration 3))
        , text " "
        , radio "decoration" "col" colp (SetDecoration (ColSelectedDecoration 2))
        , text " "
        , radio "decoration"
            "conflict"
            filledp
            (SetDecoration (AlreadyFilledDecoration ( 1, 3 )))
        ]


radio : String -> String -> Bool -> msg -> Html msg
radio group name isChecked msg =
    label []
        [ input
            [ type_ "radio"
            , Attributes.name group
            , onClick msg
            , checked isChecked
            ]
            []
        , text name
        ]
