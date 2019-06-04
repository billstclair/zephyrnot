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
import Char
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
        , button
        , div
        , fieldset
        , h1
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
import Markdown
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.LocalStorage.Sequence as Sequence exposing (KeyPair)
import PortFunnels exposing (FunnelDict, Handler(..))
import Random exposing (Seed)
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
import Time exposing (Posix)
import Url exposing (Url)
import Zephyrnot.Board as Board exposing (SizerKind(..))
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Types as Types
    exposing
        ( Board
        , Decoration(..)
        , Page(..)
        , Player(..)
        , SavedModel
        , Score
        , Winner(..)
        , zeroScore
        )


type alias SimulatorResult =
    { horizontalWins : Int
    , verticalWins : Int
    , horizontalScore : Int
    , verticalScore : Int
    }


type alias Model =
    { key : Key
    , windowSize : ( Int, Int )
    , started : Bool
    , gameCount : Int
    , gameCountString : String
    , gamesLeft : Int
    , simulatorResult : SimulatorResult
    , seed : Seed

    -- persistent below here
    , page : Page
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


type Msg
    = Noop
    | SetDecoration Decoration
    | SetChooseFirst Player
    | SetPage Page
    | ResetScore
    | NewGame
    | Click ( Int, Int )
    | SetGameCount String
    | ToggleSimulator
    | SimulatorStep
    | InitializeSeed Posix
    | WindowResize Int Int
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | ProcessLocalStorage Value


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
    , windowSize = ( 0, 0 )
    , started = False
    , gameCount = 1000
    , gameCountString = "1000"
    , gamesLeft = 0
    , simulatorResult = SimulatorResult 0 0 0 0
    , seed = Random.initialSeed 0 --get time for this
    , page = MainPage
    , decoration = NoDecoration
    , firstSelection = NoDecoration
    , chooseFirst = Zephyrus
    , player = Zephyrus
    , winner = NoWinner
    , path = []
    , moves = []
    , board = Board.empty
    , score = zeroScore
    }
        |> withCmds
            [ Task.perform getViewport Dom.getViewport
            , Task.perform InitializeSeed Time.now
            ]


getViewport : Viewport -> Msg
getViewport viewport =
    let
        vp =
            viewport.viewport
    in
    WindowResize (round vp.width) (round vp.height)


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if LocalStorage.isLoaded state.storage then
                        True

                    else
                        model.started
            }

        cmd =
            if mdl.started && not model.started then
                get pk.model

            else
                Cmd.none
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            case key of
                "model" ->
                    case value of
                        Nothing ->
                            mdl |> withCmd cmd

                        Just v ->
                            case Debug.log "decodeSavedModel" <| ED.decodeSavedModel v of
                                Err e ->
                                    mdl |> withCmd cmd

                                Ok savedModel ->
                                    savedModelToModel savedModel mdl
                                        |> withCmd cmd

                _ ->
                    mdl |> withCmd cmd

        _ ->
            mdl |> withCmd cmd


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { page = model.page
    , decoration = model.decoration
    , firstSelection = model.firstSelection
    , chooseFirst = model.chooseFirst
    , player = model.player
    , winner = model.winner
    , path = model.path
    , moves = model.moves
    , board = model.board
    , score = model.score
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | page = savedModel.page
        , decoration = savedModel.decoration
        , firstSelection = savedModel.firstSelection
        , chooseFirst = savedModel.chooseFirst
        , player = savedModel.player
        , winner = savedModel.winner
        , path = savedModel.path
        , moves = savedModel.moves
        , board = savedModel.board
        , score = savedModel.score
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( mdl, cmd ) =
            updateInternal msg model
    in
    mdl
        |> withCmds
            [ cmd
            , if model.started then
                putModel mdl

              else
                Cmd.none
            ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        Noop ->
            model |> withNoCmd

        SetDecoration decoration ->
            { model | decoration = decoration }
                |> withNoCmd

        SetChooseFirst player ->
            { model | chooseFirst = player }
                |> withNoCmd

        SetPage page ->
            { model | page = page }
                |> withNoCmd

        ResetScore ->
            { model | score = Types.zeroScore }
                |> withNoCmd

        NewGame ->
            { model
                | board = Board.empty
                , decoration = NoDecoration
                , firstSelection = NoDecoration
                , player = Zephyrus
                , winner = NoWinner
                , path = []
                , moves = []
            }
                |> withNoCmd

        Click ( row, col ) ->
            if model.winner /= NoWinner then
                model |> withNoCmd

            else
                doClick row col model

        SetGameCount string ->
            { model
                | gameCount =
                    String.toInt string
                        |> Maybe.withDefault model.gameCount
            }
                |> withNoCmd

        ToggleSimulator ->
            let
                gamesLeft =
                    if model.gamesLeft == 0 then
                        model.gameCount

                    else
                        0
            in
            { model
                | gamesLeft = gamesLeft
                , simulatorResult = SimulatorResult 0 0 0 0
            }
                |> withCmd
                    (if gamesLeft > 0 then
                        simulatorStepCmd ()

                     else
                        Cmd.none
                    )

        SimulatorStep ->
            let
                count =
                    model.gamesLeft

                loop seed left c1 c2 s1 s2 =
                    if left <= 0 then
                        ( SimulatorResult c1 c2 s1 s2, seed )

                    else
                        let
                            ( winner, score, seed2 ) =
                                Board.simulateGame seed

                            ( ( c12, c22 ), ( s12, s22 ) ) =
                                case winner of
                                    HorizontalWinner ->
                                        ( ( c1 + 1, c2 ), ( s1 + score, s2 ) )

                                    VerticalWinner ->
                                        ( ( c1, c2 + 1 ), ( s1, s2 + score ) )

                                    _ ->
                                        ( ( c1, c2 ), ( s2, s2 ) )
                        in
                        loop seed2 (left - 1) c12 c22 s12 s22

                gamesLeft =
                    model.gamesLeft - count

                oldResult =
                    model.simulatorResult

                ( newResult, newSeed ) =
                    loop model.seed count 0 0 0 0

                simulatorResult =
                    { horizontalWins =
                        oldResult.horizontalWins + newResult.horizontalWins
                    , verticalWins =
                        oldResult.verticalWins + newResult.verticalWins
                    , horizontalScore =
                        oldResult.horizontalScore + newResult.horizontalScore
                    , verticalScore =
                        oldResult.verticalScore + newResult.verticalScore
                    }
            in
            { model
                | simulatorResult = simulatorResult
                , gamesLeft = gamesLeft
                , seed = newSeed
            }
                |> withCmd
                    (if gamesLeft > 0 then
                        simulatorStepCmd ()

                     else
                        Cmd.none
                    )

        InitializeSeed posix ->
            { model
                | seed = Random.initialSeed <| Time.posixToMillis posix
            }
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

        ProcessLocalStorage value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    funnelState
                    model
            of
                Err error ->
                    -- Maybe we should display an error here,
                    -- but I don't think it will ever happen.
                    model |> withNoCmd

                Ok res ->
                    res


simulatorStepCmd : () -> Cmd Msg
simulatorStepCmd x =
    Task.perform (\_ -> SimulatorStep) <| Task.succeed x


doClick : Int -> Int -> Model -> ( Model, Cmd Msg )
doClick row col model =
    let
        makeMove r c =
            let
                filled =
                    Board.get r c model.board

                ( firstSelection, decoration, ( moves, board, player ) ) =
                    if filled then
                        ( model.firstSelection
                        , AlreadyFilledDecoration ( r, c )
                        , ( model.moves, model.board, model.player )
                        )

                    else
                        ( NoDecoration
                        , NoDecoration
                        , ( List.concat [ model.moves, [ cellName ( r, c ) ] ]
                          , Board.set r c model.board
                          , Types.otherPlayer model.player
                          )
                        )
            in
            { model
                | firstSelection = firstSelection
                , decoration = decoration
                , player = player
                , moves = moves
                , board = board
            }

        mdl =
            case model.firstSelection of
                ColSelectedDecoration selectedCol ->
                    case model.decoration of
                        RowSelectedDecoration selectedRow ->
                            if row /= selectedRow then
                                { model
                                    | decoration =
                                        RowSelectedDecoration row
                                }

                            else
                                makeMove selectedRow selectedCol

                        AlreadyFilledDecoration ( ar, ac ) ->
                            let
                                ( r, c ) =
                                    case model.player of
                                        Zephyrus ->
                                            ( ar, col )

                                        Notus ->
                                            ( row, ac )
                            in
                            if Board.get r c model.board then
                                { model
                                    | decoration =
                                        AlreadyFilledDecoration ( r, c )
                                }

                            else
                                makeMove r c

                        _ ->
                            { model
                                | decoration =
                                    RowSelectedDecoration row
                            }

                RowSelectedDecoration selectedRow ->
                    case model.decoration of
                        ColSelectedDecoration selectedCol ->
                            if col /= selectedCol then
                                { model
                                    | decoration =
                                        ColSelectedDecoration col
                                }

                            else
                                makeMove selectedRow selectedCol

                        AlreadyFilledDecoration ( ar, ac ) ->
                            let
                                ( r, c ) =
                                    case model.player of
                                        Zephyrus ->
                                            ( ar, col )

                                        Notus ->
                                            ( row, ac )
                            in
                            if Board.get r c model.board then
                                { model
                                    | decoration =
                                        AlreadyFilledDecoration ( r, c )
                                }

                            else
                                makeMove r c

                        _ ->
                            { model
                                | decoration =
                                    ColSelectedDecoration col
                            }

                _ ->
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
                            if c == col then
                                { model
                                    | decoration =
                                        NoDecoration
                                    , firstSelection =
                                        model.decoration
                                }

                            else
                                { model
                                    | decoration =
                                        ColSelectedDecoration col
                                }

                        RowSelectedDecoration r ->
                            if r == row then
                                { model
                                    | decoration =
                                        NoDecoration
                                    , firstSelection =
                                        model.decoration
                                }

                            else
                                { model
                                    | decoration =
                                        RowSelectedDecoration row
                                }

                        _ ->
                            model

        ( winner, path ) =
            Board.winner model.player mdl.board

        score =
            model.score
    in
    { mdl
        | winner = winner
        , path = path
        , score =
            case winner of
                HorizontalWinner ->
                    { score
                        | zephyrusGames =
                            score.zephyrusGames + 1
                        , zephyrusScore =
                            score.zephyrusScore + Board.score mdl.board
                    }

                VerticalWinner ->
                    { score
                        | notusGames =
                            score.notusGames + 1
                        , notusScore =
                            score.notusScore + Board.score mdl.board
                    }

                _ ->
                    score
    }
        |> withNoCmd


cellName : ( Int, Int ) -> String
cellName ( rowidx, colidx ) =
    Board.colToString colidx ++ Board.rowToString rowidx


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResize
        , PortFunnels.subscriptions ProcessLocalStorage model
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


herculanumStyle : Attribute msg
herculanumStyle =
    style "font-family" "Herculanum, sans-serif"


view : Model -> Document Msg
view model =
    let
        bsize =
            boardSize model
    in
    { title = "ZEPHYRNOT"
    , body =
        [ if bsize == 0 then
            text ""

          else
            div []
                [ div
                    [ align "center"
                    ]
                    [ h1
                        [ style "margin" "0 0 0.2em 0"
                        , herculanumStyle
                        ]
                        [ text "Zephyrnot" ]
                    , h2
                        [ style "margin" "0 0 0.2em 0"
                        , herculanumStyle
                        ]
                        [ text "Feud of the Winds" ]
                    , p [ style "margin" "0" ]
                        [ text "Invented by Chris St. Clair" ]
                    ]
                , case model.page of
                    MainPage ->
                        mainPage bsize model

                    RulesPage ->
                        rulesPage bsize model

                    InstructionsPage ->
                        instructionsPage bsize model

                    AuxPage ->
                        auxPage bsize model
                ]
        ]
    }


mainPage : Int -> Model -> Html Msg
mainPage bsize model =
    let
        score =
            model.score

        count =
            Board.count model.board

        message =
            case model.winner of
                HorizontalWinner ->
                    "Zephyrus wins in " ++ String.fromInt count ++ "!"

                VerticalWinner ->
                    "Notus wins in " ++ String.fromInt count ++ "!"

                NoWinner ->
                    case model.firstSelection of
                        RowSelectedDecoration _ ->
                            case model.decoration of
                                NoDecoration ->
                                    "Notus chose. Zephyrus pick a column"

                                AlreadyFilledDecoration _ ->
                                    case model.player of
                                        Zephyrus ->
                                            "Zephyrus pick another column"

                                        Notus ->
                                            "Notus pick another row"

                                _ ->
                                    "Notus chose. Zephyrus confirm or pick another column"

                        ColSelectedDecoration _ ->
                            case model.decoration of
                                NoDecoration ->
                                    "Zephyrus chose. Notus pick a row"

                                AlreadyFilledDecoration _ ->
                                    case model.player of
                                        Zephyrus ->
                                            "Zephyrus pick another column"

                                        Notus ->
                                            "Notus pick another row"

                                _ ->
                                    "Zephyrus chose. Notus confirm or pick another row"

                        _ ->
                            case model.decoration of
                                NoDecoration ->
                                    if model.chooseFirst == Zephyrus then
                                        "Zephyrus pick a column"

                                    else
                                        "Notus pick a row"

                                ColSelectedDecoration _ ->
                                    "Zephyrus confirm or pick another column"

                                RowSelectedDecoration _ ->
                                    "Notus confirm or pick another row"

                                AlreadyFilledDecoration _ ->
                                    if model.player == Zephyrus then
                                        "Zephyrus pick another column"

                                    else
                                        "Notus pick another row"
    in
    div [ align "center" ]
        [ Board.render bsize
            Click
            (Just <| Board.getSizer DefaultSizer)
            model.decoration
            model.path
            model.board
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
                    [ b "Stone Placer: "
                    , text <|
                        case model.player of
                            Zephyrus ->
                                "Zephyrus"

                            Notus ->
                                "Notus"
                    ]
            , br
            , b "Choose first: "
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
            , text "Zephyrus/Notus, points: "
            , text <| String.fromInt score.zephyrusScore
            , text "/"
            , text <| String.fromInt score.notusScore
            , text ", games: "
            , text <| String.fromInt score.zephyrusGames
            , text "/"
            , text <| String.fromInt score.notusGames
            , text " "
            , button [ onClick ResetScore ]
                [ text "Reset" ]
            , br
            , button
                [ onClick NewGame ]
                [ text "New Game" ]
            ]
        , p []
            [ text "Moves: "
            , text <| movesToString model.moves
            ]
        , p []
            [ a
                [ href "#"
                , onClick <| SetPage InstructionsPage
                ]
                [ text "Instructions" ]
            , text " "
            , a
                [ href "#"
                , onClick <| SetPage AuxPage
                ]
                [ text "Aux" ]
            , text " "
            , a
                [ href "#"
                , onClick <| SetPage RulesPage
                ]
                [ text "Rules" ]
            , br
            , a
                [ href "https://github.com/billstclair/zephyrnot/"
                , target "_blank"
                ]
                [ text "GitHub" ]
            ]
        ]


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
pairToString ( s1, s2 ) =
    if s2 == "" then
        s1

    else
        "(" ++ s1 ++ "," ++ chars.nbsp ++ s2 ++ ")"



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


rulesDiv : Bool -> List (Html Msg) -> Html Msg
rulesDiv alignCenter body =
    div
        [ style "width" "25em"
        , if alignCenter then
            align "center"

          else
            style "margin" "auto"
        ]
        body


playButton : Html Msg
playButton =
    rulesDiv True
        [ button
            [ onClick <| SetPage MainPage
            , style "font-size" "110%"
            ]
            [ text "Play" ]
        ]


instructionsPage : Int -> Model -> Html Msg
instructionsPage bsize model =
    rulesDiv False
        [ br
        , playButton
        , rulesDiv True
            [ br, b "Instructions" ]
        , rulesDiv False
            [ Markdown.toHtml [] """
The players must choose who will be Zephyrus, placing the first stone,
and attempting to create a path connecting the east and west
edges, and who will be Notus, placing the second stone, and attempting
to create a path connecting the north and south edges.

Zephyrus taps a column to select it, then taps another column to
change the selection, or the same column again to choose it.

Notus taps a row to select it, then taps another row to
change the selection, or the same column again to choose it.

After both players have made their selection, the stone at the
intersection of the selected row and column is placed. If there is
already a stone there, it is highlighted in red, and the player
placing the stone must choose another row or column that is
unoccupied.

Play continues until there is a path connecting two opposite
edges. Click the "New Game" button to play again.

In non-networked play, the "Choose first" radio buttons control which
player chooses a row or column first for each stone placement. This is
amenable to using a portable device (e.g. a smart phone or tablet),
and passing it back and forth to make selections.
"""
            ]
        , playButton
        ]


rulesPage : Int -> Model -> Html Msg
rulesPage bsize model =
    rulesDiv False
        [ br
        , playButton
        , rulesDiv True
            [ br, b "Rules" ]
        , rulesDiv False
            [ Markdown.toHtml [] """ 
One player is "placing the stone" on each round. Zephyrus starts, and
play alternates between Zephyrus and Notus. Zephyrus controls the west
wind, the left edge of the board. Notus controls the south wind, the
bottom edge of the board.

Each round, players simultaneously and secretly declare the rank or
file (row or column) they want the next stone to be placed
on. Zephyrus chooses the file ("a" being furthest west, and "f" being
furthest east), and Notus chooses the rank (1 being furthest south,
and 6 being furthest north).

If players declare an occupied point, the player placing the stone
must change their choice to declare an empty point instead (their
opponent’s declared choice remains the same).

Each player is trying to connect their edge of the board to its
opposite edge with an unbroken path of orthogonally adjacent stones
(west to east or south to north). The first player to do so wins. The
path does not need to be a straight line. If a path is made between
all 4 sides on the same turn, the player who placed the final stone
wins.
"""
            ]
        , playButton
        , rulesDiv False
            [ h2 [ herculanumStyle, align "center" ]
                [ text "The Anemoi: Zephyrus and Notus" ]
            , Markdown.toHtml []
                """
In Greek mythology, the sibling gods the Anemoi personify the winds of the cardinal
directions. They bring the changing of the weather and seasons and control many
aspects of daily life.

Zephyrus, the beneficial god of the west wind, brings gentle rains and warm breezes,
heralding spring and the blooming of the land.

Notus, the tempestuous god of the south wind, brings thick mists and summer storms,
concealing thieves and destroying crops.

Their brothers Boreas and Eurus are the north winter wind and the unlucky east wind,
respectively.

In Zephyrnot, players take the rolls of Zephyrus and Notus, vying for control of the
prevailing winds of Greece. But while you may have power over the winds from your
compass point, take care: your opponent’s hidden gusts will directly affect your every
move. Will you be able to predict the wind?
"""
            ]
        , rulesDiv True
            [ h2 [ herculanumStyle, align "center" ]
                [ text "Pronunciation" ]
            , p
                []
                [ text "Zephyrnot ["
                , b "zef"
                , text "-er-noht]"
                , br
                , text "Anemoi ["
                , b "an"
                , text "-em-oy]"
                , br
                , text "Zephyrus ["
                , b "zef"
                , text "-er-uh s]"
                , br
                , text "Notus ["
                , b "noh"
                , text "-tuh s]"
                , br
                , text "Boreas ["
                , b "bawr"
                , text "-ee-uh s]"
                , br
                , text "Eurus ["
                , b "yoor"
                , text "-uh s]"
                ]
            ]
        , playButton
        ]


auxPage : Int -> Model -> Html Msg
auxPage bsize model =
    let
        { horizontalWins, verticalWins, horizontalScore, verticalScore } =
            model.simulatorResult

        locale =
            { usLocale | decimals = 1 }

        hscore =
            (toFloat horizontalScore / toFloat horizontalWins)
                |> format locale

        vscore =
            (toFloat verticalScore / toFloat verticalWins)
                |> format locale

        percent =
            100 * horizontalWins // (horizontalWins + verticalWins)
    in
    rulesDiv False
        [ br
        , playButton
        , rulesDiv True
            [ br, b "Aux" ]
        , p [ align "center" ]
            [ b "Game Count: "
            , input
                [ onInput SetGameCount
                , value model.gameCountString
                , size 4
                ]
                []
            , text " "
            , button
                [ onClick ToggleSimulator ]
                [ if model.gamesLeft > 0 then
                    text "Stop"

                  else
                    text "Simulate"
                ]
            , br
            , if horizontalWins == 0 && verticalWins == 0 then
                text ""

              else
                span []
                    [ text "Zephyrus/Notus points: "
                    , text hscore
                    , text "/"
                    , text vscore
                    , text ", games: "
                    , text <| String.fromInt horizontalWins
                    , text "/"
                    , text <| String.fromInt verticalWins
                    , text ", "
                    , text <| String.fromInt percent
                    , text "/"
                    , text <| String.fromInt (100 - percent)
                    , text "%"
                    ]
            ]
        ]


b : String -> Html msg
b s =
    Html.b [] [ text s ]


codestr : Int -> String
codestr code =
    String.fromList [ Char.fromCode code ]


chars =
    { leftCurlyQuote = codestr 0x201C
    , copyright = codestr 0xA9
    , nbsp = codestr 0xA0
    }



---
--- Persistence
---


putModel : Model -> Cmd Msg
putModel model =
    let
        savedModel =
            modelToSavedModel model

        value =
            ED.encodeSavedModel savedModel
    in
    put pk.model <| Just value


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put key value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get key)


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        funnelState.storage


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort ProcessLocalStorage moduleName False


localStoragePrefix : String
localStoragePrefix =
    "zephyrnot"


funnelState : PortFunnels.State
funnelState =
    PortFunnels.initialState localStoragePrefix


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler ]
        getCmdPort


pk =
    { model = "model"
    }
