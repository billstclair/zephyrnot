---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- Zephyrnot JSON encoders and decoders
-- Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Zephyrnot.EncodeDecode exposing (decodeSavedModel, encodeSavedModel)

import Array exposing (Array)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (optional, required)
import Json.Encode as JE exposing (Value)
import Zephyrnot.Types as Types
    exposing
        ( Board
        , Decoration(..)
        , Page(..)
        , Player(..)
        , SavedModel
        , Score
        , Winner(..)
        )


encodeMoves : List String -> Value
encodeMoves moves =
    moves
        |> List.intersperse ","
        |> String.concat
        |> JE.string


movesDecoder : Decoder (List String)
movesDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (String.split "," >> JD.succeed)
        , JD.list JD.string --backward compatibility
        ]


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "page", encodePage model.page )
        , ( "decoration", encodeDecoration model.decoration )
        , ( "firstSelection", encodeDecoration model.firstSelection )
        , ( "chooseFirst", encodePlayer model.chooseFirst )
        , ( "player", encodePlayer model.player )
        , ( "winner", encodeWinner model.winner )
        , ( "path", JE.list encodeIntPair model.path )
        , ( "moves", encodeMoves model.moves )
        , ( "board", encodeBoard model.board )
        , ( "score", encodeScore model.score )
        ]


decodeSavedModel : Value -> Result JD.Error SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "page" pageDecoder MainPage
        |> required "decoration" decorationDecoder
        |> required "firstSelection" decorationDecoder
        |> required "chooseFirst" playerDecoder
        |> required "player" playerDecoder
        |> required "winner" winnerDecoder
        |> required "path" (JD.list intPairDecoder)
        |> required "moves" movesDecoder
        |> required "board" boardDecoder
        |> optional "score" scoreDecoder Types.zeroScore


encodePage : Page -> Value
encodePage page =
    JE.string <|
        case page of
            MainPage ->
                "MainPage"

            RulesPage ->
                "RulesPage"

            InstructionsPage ->
                "InstructionsPage"

            AuxPage ->
                "AuxPage"


pageDecoder : Decoder Page
pageDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "MainPage" ->
                        JD.succeed MainPage

                    "RulesPage" ->
                        JD.succeed RulesPage

                    "InstructionsPage" ->
                        JD.succeed InstructionsPage

                    "AuxPage" ->
                        JD.succeed AuxPage

                    _ ->
                        JD.fail <| "Unknown page: " ++ s
            )


encodeDecoration : Decoration -> Value
encodeDecoration decoration =
    case decoration of
        NoDecoration ->
            JE.string "NoDecoration"

        RowSelectedDecoration rowidx ->
            JE.object [ ( "RowSelectedDecoration", JE.int rowidx ) ]

        ColSelectedDecoration colidx ->
            JE.object [ ( "ColSelectedDecoration", JE.int colidx ) ]

        AlreadyFilledDecoration pair ->
            JE.object [ ( "AlreadyFilledDecoration", encodeIntPair pair ) ]


decorationDecoder : Decoder Decoration
decorationDecoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\s ->
                    if s == "NoDecoration" then
                        JD.succeed NoDecoration

                    else
                        JD.fail <| "Unknown Decoration: " ++ s
                )
        , JD.field "RowSelectedDecoration" JD.int
            |> JD.andThen
                (\rowidx ->
                    JD.succeed <| RowSelectedDecoration rowidx
                )
        , JD.field "ColSelectedDecoration" JD.int
            |> JD.andThen
                (\colidx ->
                    JD.succeed <| ColSelectedDecoration colidx
                )
        , JD.field "AlreadyFilledDecoration" intPairDecoder
            |> JD.andThen
                (\pair ->
                    JD.succeed <| AlreadyFilledDecoration pair
                )
        ]


encodePlayer : Player -> Value
encodePlayer player =
    case player of
        Zephyrus ->
            JE.string "Zephyrus"

        Notus ->
            JE.string "Notus"


playerDecoder : Decoder Player
playerDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "Zephyrus" ->
                        JD.succeed Zephyrus

                    "Notus" ->
                        JD.succeed Notus

                    _ ->
                        JD.fail <| "Unknown player: " ++ s
            )


encodeWinner : Winner -> Value
encodeWinner winner =
    JE.string <|
        case winner of
            NoWinner ->
                "NoWinner"

            HorizontalWinner ->
                "HorizontalWinner"

            VerticalWinner ->
                "VerticalWinner"


winnerDecoder : Decoder Winner
winnerDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "NoWinner" ->
                        JD.succeed NoWinner

                    "HorizontalWinner" ->
                        JD.succeed HorizontalWinner

                    "VerticalWinner" ->
                        JD.succeed VerticalWinner

                    _ ->
                        JD.fail <| "Unknown winner: " ++ s
            )


encodeIntPair : ( Int, Int ) -> Value
encodeIntPair ( x, y ) =
    JE.list JE.int [ x, y ]


intPairDecoder : Decoder ( Int, Int )
intPairDecoder =
    JD.list JD.int
        |> JD.andThen
            (\list ->
                case list of
                    [ rowidx, colidx ] ->
                        JD.succeed ( rowidx, colidx )

                    _ ->
                        JD.fail "Wrong length Int pair"
            )


boolToString : Bool -> String
boolToString bool =
    if bool then
        "0"

    else
        "-"


stringToBool : String -> Bool
stringToBool string =
    string == "0"


rowToString : Array Bool -> String
rowToString row =
    Array.toList row
        |> List.map boolToString
        |> String.concat


stringToRow : String -> Array Bool
stringToRow string =
    String.toList string
        |> List.map String.fromChar
        |> List.map stringToBool
        |> Array.fromList


boardToString : Board -> String
boardToString board =
    Array.toList board
        |> List.map rowToString
        |> List.intersperse "|"
        |> String.concat


stringToBoard : String -> Maybe Board
stringToBoard string =
    if String.length string /= 41 then
        Nothing

    else
        let
            rows =
                [ String.slice 0 6 string
                , String.slice 7 13 string
                , String.slice 14 20 string
                , String.slice 21 27 string
                , String.slice 28 34 string
                , String.slice 35 41 string
                ]
        in
        rows
            |> List.map stringToRow
            |> Array.fromList
            |> Just


encodeBoard : Board -> Value
encodeBoard board =
    JE.string <| boardToString board


newBoardDecoder : Decoder Board
newBoardDecoder =
    JD.string
        |> JD.andThen
            (\string ->
                case stringToBoard string of
                    Nothing ->
                        JD.fail "Invalid board string."

                    Just board ->
                        JD.succeed board
            )


boardDecoder : Decoder Board
boardDecoder =
    JD.oneOf
        [ oldBoardDecoder
        , newBoardDecoder
        ]


oldBoardDecoder : Decoder Board
oldBoardDecoder =
    JD.list (JD.list JD.bool)
        |> JD.andThen
            (\l ->
                List.map (\l2 -> Array.fromList l2) l
                    |> Array.fromList
                    |> JD.succeed
            )


encodeScore : Score -> Value
encodeScore score =
    JE.object
        [ ( "zephyrusGames", JE.int score.zephyrusGames )
        , ( "notusGames", JE.int score.notusGames )
        , ( "zephyrusScore", JE.int score.zephyrusScore )
        , ( "notusScore", JE.int score.notusScore )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    JD.succeed Score
        |> required "zephyrusGames" JD.int
        |> required "notusGames" JD.int
        |> required "zephyrusScore" JD.int
        |> required "notusScore" JD.int
