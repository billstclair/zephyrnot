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
import Json.Encode as JE exposing (Value)
import Zephyrnot.Types
    exposing
        ( Board
        , Decoration(..)
        , Player(..)
        , SavedModel
        , Winner(..)
        )


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "decoration", encodeDecoration model.decoration )
        , ( "firstSelection", encodeDecoration model.firstSelection )
        , ( "chooseFirst", encodePlayer model.chooseFirst )
        , ( "player", encodePlayer model.player )
        , ( "winner", encodeWinner model.winner )
        , ( "path", JE.list encodeIntPair model.path )
        , ( "moves", JE.list JE.string model.moves )
        , ( "board", encodeBoard model.board )
        ]


decodeSavedModel : Value -> Result JD.Error SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.map8 SavedModel
        (JD.field "decoration" decorationDecoder)
        (JD.field "firstSelection" decorationDecoder)
        (JD.field "chooseFirst" playerDecoder)
        (JD.field "player" playerDecoder)
        (JD.field "winner" winnerDecoder)
        (JD.field "path" <| JD.list intPairDecoder)
        (JD.field "moves" <| JD.list JD.string)
        (JD.field "board" boardDecoder)


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


encodeBoard : Board -> Value
encodeBoard board =
    JE.array (JE.array JE.bool) board


boardDecoder : Decoder Board
boardDecoder =
    JD.list (JD.list JD.bool)
        |> JD.andThen
            (\l ->
                List.map (\l2 -> Array.fromList l2) l
                    |> Array.fromList
                    |> JD.succeed
            )
