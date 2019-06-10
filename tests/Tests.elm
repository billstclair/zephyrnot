module Tests exposing (all)

import Dict
import Expect exposing (Expectation)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List
import Maybe exposing (withDefault)
import Test exposing (..)
import Zephyrnot.Board as Board
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Types as Types
    exposing
        ( Board
        , Choice(..)
        , Decoration(..)
        , GameState
        , Message(..)
        , Player(..)
        , PlayerNames
        , PublicGame
        , Score
        , Winner(..)
        )


log =
    Debug.log


enableLogging : Bool
enableLogging =
    False



--change to True to log JSON input & output results


maybeLog : String -> a -> a
maybeLog label value =
    if enableLogging then
        log label value

    else
        value


testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let
        numbers =
            List.map Debug.toString <| List.range 1 (List.length data)
    in
    List.map2 test data numbers


all : Test
all =
    Test.concat <|
        List.concat
            [ testMap protocolTest protocolData
            , testMap boardTest boardData
            , testMap gameStateTest gameStateData
            ]


expectResult : Result String thing -> Result String thing -> Expectation
expectResult sb was =
    case maybeLog "  result" was of
        Err msg ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false msg True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


protocolTest : Message -> String -> Test
protocolTest message name =
    test ("protocolTest \"" ++ name ++ "\"")
        (\_ ->
            let
                pair =
                    maybeLog "protocolJson" <| ED.messageEncoderWithPrivate message
            in
            expectResult (Ok message) <| ED.messageDecoder pair
        )


protocolData : List Message
protocolData =
    [ NewReq
        { name = "Bill"
        , player = Zephyrus
        , isPublic = True
        , restoreState = Nothing
        }
    , NewReq
        { name = "Joe"
        , player = Notus
        , isPublic = False
        , restoreState = Just gameState1
        }
    , NewRsp
        { gameid = "123"
        , playerid = "76"
        , player = Zephyrus
        , name = "Joe"
        , gameState = gameState1
        }
    , NewRsp
        { gameid = "123a"
        , playerid = "76b"
        , player = Notus
        , name = "Joel"
        , gameState = gameState2
        }
    , JoinReq
        { gameid = "123"
        , name = "Irving"
        }
    , ReJoinReq
        { gameid = "123"
        , playerid = "76"
        }
    , JoinRsp
        { gameid = "123"
        , playerid = Just "77"
        , player = Notus
        , gameState = gameState2
        }
    , JoinRsp
        { gameid = "123"
        , playerid = Nothing
        , player = Notus
        , gameState = gameState2
        }
    , LeaveReq { playerid = "77" }
    , LeaveRsp { gameid = "123", player = Zephyrus }
    , LeaveRsp { gameid = "123", player = Notus }
    , UpdateReq { playerid = "77" }
    , UpdateRsp
        { gameid = "123"
        , gameState = gameState1
        }
    , PlayReq
        { playerid = "77"
        , placement = ChooseRow 2
        }
    , PlayReq
        { playerid = "78"
        , placement = ChooseCol 3
        }
    , PlayReq
        { playerid = "79"
        , placement = ChooseResign Zephyrus
        }
    , PlayReq
        { playerid = "79"
        , placement = ChooseResign Notus
        }
    , PlayReq
        { playerid = "80"
        , placement = ChooseNew Zephyrus
        }
    , PlayReq
        { playerid = "80"
        , placement = ChooseNew Notus
        }
    , PlayRsp
        { gameid = "77"
        , gameState = gameState1
        , decoration = RowSelectedDecoration 2
        }
    , PlayRsp
        { gameid = "78"
        , gameState = gameState2
        , decoration = ColSelectedDecoration 2
        }
    , PlayRsp
        { gameid = "78"
        , gameState = gameState4
        , decoration = gameState4.private.decoration
        }
    , ResignRsp
        { gameid = "79"
        , gameState = { gameState3 | winner = NotusWinner }
        , player = Zephyrus
        }
    , ResignRsp
        { gameid = "79"
        , gameState = { gameState3 | winner = ZephyrusWinner }
        , player = Notus
        }
    , AnotherGameRsp
        { gameid = "80"
        , gameState = gameState1
        , player = Zephyrus
        }
    , AnotherGameRsp
        { gameid = "80"
        , gameState = gameState2
        , player = Notus
        }
    , GameOverRsp
        { gameid = "80"
        , gameState = { gameState1 | winner = ZephyrusWinner }
        }
    , GameOverRsp
        { gameid = "80"
        , gameState = { gameState2 | winner = NotusWinner }
        }
    , PublicGamesReq
        { subscribe = False
        , forName = Nothing
        }
    , PublicGamesReq
        { subscribe = True
        , forName = Just "Bill"
        }
    , PublicGamesRsp
        { games = [] }
    , PublicGamesRsp
        { games = [ publicGame1, publicGame2 ] }
    , PublicGamesUpdateRsp
        { added = [ publicGame1, publicGame2 ]
        , removed = []
        }
    , PublicGamesUpdateRsp
        { added = []
        , removed = [ "foo", "bar" ]
        }
    , ErrorRsp
        { request = "request"
        , text = "text"
        }
    , ChatReq
        { playerid = "77"
        , text = "text"
        }
    , ChatRsp
        { gameid = "123"
        , name = "Bob"
        , text = "text"
        }
    ]


expectString : String -> String -> Expectation
expectString sb was =
    Expect.equal sb was


boardTest : String -> String -> Test
boardTest encodedBoard name =
    test ("boardTest \"" ++ name ++ "\"")
        (\_ ->
            let
                board =
                    ED.stringToBoard encodedBoard

                boardString =
                    case board of
                        Nothing ->
                            ""

                        Just b ->
                            ED.boardToString b
            in
            expectString encodedBoard boardString
        )


board1String =
    "0-----|-0----|--0---|---0--|----0-|-----0"


board2String =
    "----00|---00-|--00--|-00---|00----|0----0"


decodeBoard : String -> Board
decodeBoard string =
    ED.stringToBoard string
        |> Maybe.withDefault Board.empty


board1 =
    decodeBoard board1String


board2 =
    decodeBoard board2String


boardData : List String
boardData =
    [ board1String
    , board2String
    ]


decodeValue : Decoder a -> Value -> Result String a
decodeValue decoder value =
    case JD.decodeValue decoder value of
        Ok a ->
            Ok a

        Err err ->
            Err <| JD.errorToString err


gameStateTest : GameState -> String -> Test
gameStateTest gameState name =
    test ("gameStateTest \"" ++ name ++ "\"")
        (\_ ->
            let
                value =
                    maybeLog "gameState" <| ED.encodeGameState True gameState
            in
            expectResult (Ok gameState) <|
                decodeValue ED.gameStateDecoder value
        )


players1 =
    PlayerNames "Billy Bob" "Bobby Sue"


players2 =
    PlayerNames "Joe" "Random"


score1 =
    Score 0 1 2 3


score2 =
    Score 4 5 6 7


gameState1 =
    { board = board1
    , moves = [ "a1", "b2", "c3" ]
    , players = players1
    , whoseTurn = Zephyrus
    , score = score1
    , winner = NoWinner
    , path = []
    , private = { decoration = RowSelectedDecoration 0 }
    }


gameState2 =
    { board = board2
    , moves = [ "a1", "b2", "c3", "d4" ]
    , players = players2
    , whoseTurn = Notus
    , score = score1
    , winner = ZephyrusWinner
    , path = [ ( 1, 2 ), ( 2, 3 ) ]
    , private = { decoration = ColSelectedDecoration 2 }
    }


gameState3 =
    { board = board1
    , moves = [ "a1", "b2", "c3", "d4", "e5", "f6" ]
    , players = players2
    , whoseTurn = Notus
    , score = score2
    , winner = NotusWinner
    , path = [ ( 1, 2 ), ( 2, 3 ) ]
    , private = { decoration = NoDecoration }
    }


gameState4 =
    { gameState3 | private = { decoration = AlreadyFilledDecoration ( 0, 2 ) } }


gameStateData : List GameState
gameStateData =
    [ gameState1
    , gameState2
    , gameState3
    ]


publicGame1 : PublicGame
publicGame1 =
    { gameid = "foo"
    , creator = "Bill"
    , player = Zephyrus
    , forName = Nothing
    }


publicGame2 : PublicGame
publicGame2 =
    { gameid = "bar"
    , creator = "Chris"
    , player = Notus
    , forName = Just "Bill"
    }
