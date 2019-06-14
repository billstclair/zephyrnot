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
import ElmChat exposing (LineSpec(..), defaultExtraAttributes)
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
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Markdown
import PortFunnel.LocalStorage as LocalStorage
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
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
import Time exposing (Posix, Zone)
import Url exposing (Url)
import WebSocketFramework
import WebSocketFramework.EncodeDecode as WSFED
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( EncodeDecode
        , GameId
        , MessageDecoder
        , MessageEncoder
        , PlayerId
        , ReqRsp(..)
        , ServerInterface(..)
        , ServerMessageProcessor
        )
import Zephyrnot.Board as Board exposing (SizerKind(..))
import Zephyrnot.EncodeDecode as ED
import Zephyrnot.Interface as Interface
import Zephyrnot.Types as Types
    exposing
        ( Board
        , Choice(..)
        , Decoration(..)
        , GameState
        , Message(..)
        , Page(..)
        , Player(..)
        , PlayerNames
        , PublicGame
        , PublicType(..)
        , SavedModel
        , Score
        , Settings
        , Winner(..)
        , zeroScore
        )
import Zephyrnot.WhichServer as WhichServer


type alias SimulatorState =
    { gameCount : Int
    , gameCountString : String
    , gamesLeft : Int
    , simulatorResult : SimulatorResult
    }


type alias SimulatorResult =
    { horizontalWins : Int
    , verticalWins : Int
    , horizontalScore : Int
    , verticalScore : Int
    }


type alias ServerInterface =
    WebSocketFramework.Types.ServerInterface GameState Player Message Msg


type ConnectionReason
    = NoConnection
    | StartGameConnection
    | JoinGameConnection
    | PublicGamesConnection
    | UpdateConnection


type alias ChatSettings =
    ElmChat.Settings Msg


type alias Model =
    { serverUrl : String
    , interface : ServerInterface
    , connectionReason : ConnectionReason
    , funnelState : State
    , otherPlayerid : PlayerId
    , key : Key
    , windowSize : ( Int, Int )
    , started : Bool --True when persistent storage is available
    , simulatorState : SimulatorState --for the "Aux"  page
    , seed : Seed
    , error : Maybe String
    , chatSettings : ChatSettings
    , publicGames : List PublicGame
    , time : Posix

    -- persistent below here
    , page : Page
    , decoration : Decoration
    , firstSelection : Decoration
    , chooseFirst : Player
    , player : Player
    , gameState : GameState
    , isLocal : Bool
    , gameid : String
    , playerid : PlayerId
    , isLive : Bool
    , settings : Settings
    }


isPlaying : Model -> Bool
isPlaying model =
    let
        { zephyrus, notus } =
            model.gameState.players
    in
    model.isLive && zephyrus /= "" && notus /= ""


type Msg
    = Noop
    | IncomingMessage ServerInterface Message
    | SetDecoration Decoration
    | SetChooseFirst Player
    | SetIsLocal Bool
    | SetName String
    | SetIsPublic Bool
    | SetForName String
    | SetServerUrl String
    | SetGameid String
    | SetPage Page
    | SetHideTitle Bool
    | ResetScore
    | NewGame
    | StartGame
    | Join
    | JoinGame GameId
    | Disconnect
    | ClearStorage
    | Click ( Int, Int )
    | ChatUpdate ChatSettings (Cmd Msg)
    | ChatSend String ChatSettings
    | ChatClear
    | DelayedAction (Model -> ( Model, Cmd Msg )) Posix
    | SetZone Zone
    | SetGameCount String
    | ToggleSimulator
    | SimulatorStep
    | InitializeSeed Posix
    | WindowResize Int Int
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url
    | Process Value


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


encodeDecode : EncodeDecode Message
encodeDecode =
    { encoder = ED.messageEncoderWithPrivate
    , decoder = ED.messageDecoder
    , errorWrapper = Nothing
    }


fullProcessor : ServerMessageProcessor GameState Player Message
fullProcessor =
    ServerInterface.fullMessageProcessor encodeDecode Interface.messageProcessor


proxyServer : ServerInterface
proxyServer =
    ServerInterface.makeProxyServer fullProcessor IncomingMessage


updateChatAttributes : ChatSettings -> ChatSettings
updateChatAttributes settings =
    let
        attributes =
            settings.attributes
    in
    { settings
        | attributes =
            { attributes
                | chatTable =
                    [ style "width" "90%" ]
                , textColumn =
                    [ style "width" "100%" ]
                , textArea =
                    [ style "width" "100%"
                    , style "height" "6em"
                    ]
            }
    }


initialChatSettings : ChatSettings
initialChatSettings =
    ElmChat.makeSettings ids.chatOutput 14 True ChatUpdate
        |> updateChatAttributes


init : Value -> url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { serverUrl = WhichServer.serverUrl
            , interface = proxyServer
            , connectionReason = NoConnection
            , funnelState = initialFunnelState
            , otherPlayerid = ""
            , key = key
            , windowSize = ( 0, 0 )
            , started = False
            , simulatorState =
                { gameCount = 1000
                , gameCountString = "1000"
                , gamesLeft = 0
                , simulatorResult = SimulatorResult 0 0 0 0
                }
            , seed = Random.initialSeed 0 --get time for this
            , error = Nothing
            , chatSettings = initialChatSettings
            , publicGames = []
            , time = Time.millisToPosix 0

            -- persistent fields
            , page = MainPage
            , decoration = NoDecoration
            , firstSelection = NoDecoration
            , chooseFirst = Zephyrus
            , player = Zephyrus
            , gameState = Interface.emptyGameState (PlayerNames "" "")
            , isLocal = False
            , gameid = ""
            , playerid = ""
            , isLive = False
            , settings = Types.emptySettings
            }
    in
    model
        |> withCmds
            [ Task.perform getViewport Dom.getViewport
            , Task.perform InitializeSeed Time.now
            , Task.perform SetZone Time.here
            ]


type alias NewReqBody =
    { name : String
    , player : Player
    , publicType : PublicType
    , restoreState : Maybe GameState
    }


initialNewReqBody : NewReqBody
initialNewReqBody =
    { name = "Zephyrus"
    , player = Zephyrus
    , publicType = NotPublic
    , restoreState = Nothing
    }


initialNewReq : Message
initialNewReq =
    NewReq initialNewReqBody


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
                Cmd.batch
                    [ get pk.model
                    , get pk.chat
                    ]

            else
                Cmd.none
    in
    case response of
        LocalStorage.GetResponse { label, key, value } ->
            case value of
                Nothing ->
                    mdl |> withNoCmd

                Just v ->
                    handleGetResponse key v model

        _ ->
            mdl |> withCmd cmd


handleGetResponse : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetResponse key value model =
    if key == pk.chat then
        case JD.decodeValue (ElmChat.settingsDecoder ChatUpdate) value of
            Err _ ->
                model |> withNoCmd

            Ok settings ->
                let
                    chatSettings =
                        updateChatAttributes
                            { settings | id = ids.chatOutput }
                in
                { model
                    | chatSettings = chatSettings
                }
                    |> withCmd (ElmChat.restoreScroll chatSettings)

    else if key == pk.model then
        case Debug.log "decodeSavedModel" <| ED.decodeSavedModel value of
            Err e ->
                model |> withNoCmd

            Ok savedModel ->
                let
                    model2 =
                        savedModelToModel savedModel model
                in
                if not model2.isLocal && model2.isLive && model2.playerid /= "" then
                    model2
                        |> webSocketConnect UpdateConnection

                else if not model2.isLocal && model2.page == PublicPage then
                    { model2 | gameid = "" }
                        |> webSocketConnect PublicGamesConnection

                else if model2.isLocal then
                    { model2 | gameid = "" }
                        |> withCmd
                            (send model2 <|
                                NewReq
                                    { initialNewReqBody
                                        | restoreState =
                                            Just model2.gameState
                                    }
                            )

                else
                    model2 |> withNoCmd

    else
        model |> withNoCmd


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { page = model.page
    , decoration = model.decoration
    , firstSelection = model.firstSelection
    , chooseFirst = model.chooseFirst
    , player = model.player
    , gameState = model.gameState
    , isLocal = model.isLocal
    , isLive = model.isLive
    , gameid = model.gameid
    , playerid = model.playerid
    , settings = model.settings
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel savedModel model =
    { model
        | page = savedModel.page
        , decoration = savedModel.decoration
        , firstSelection = savedModel.firstSelection
        , chooseFirst = savedModel.chooseFirst
        , player = savedModel.player
        , gameState = savedModel.gameState
        , isLocal = savedModel.isLocal
        , isLive = savedModel.isLive
        , gameid = savedModel.gameid
        , playerid = savedModel.playerid
        , settings = savedModel.settings
        , interface = proxyServer
    }


playerName : Player -> Model -> String
playerName player model =
    let
        players =
            model.gameState.players
    in
    case player of
        Zephyrus ->
            players.zephyrus

        Notus ->
            players.notus


incomingMessage : ServerInterface -> Message -> Model -> ( Model, Cmd Msg )
incomingMessage interface message mdl =
    let
        model =
            { mdl | interface = interface }
    in
    case Debug.log "incomingMessage" message of
        NewRsp { gameid, playerid, player, name, gameState } ->
            { model
                | gameid = gameid
                , playerid = playerid
                , isLive = True
                , connectionReason = JoinGameConnection
                , gameState = gameState
                , decoration = gameState.private.decoration
            }
                |> withCmd
                    (if not model.isLocal then
                        Cmd.none

                     else if player == Zephyrus then
                        send model <|
                            JoinReq { gameid = gameid, name = "Notus" }

                     else
                        send model <|
                            JoinReq { gameid = gameid, name = "Zephyrus" }
                    )

        JoinRsp { gameid, playerid, player, gameState } ->
            let
                chatSettings =
                    model.chatSettings

                model2 =
                    { model
                        | page = MainPage
                        , gameState = gameState
                        , isLive = True
                        , connectionReason = NoConnection
                        , decoration = gameState.private.decoration
                        , chooseFirst =
                            if playerid == Nothing then
                                model.chooseFirst

                            else
                                player
                        , chatSettings =
                            { chatSettings
                                | lines = []
                                , input = ""
                            }
                    }

                model3 =
                    if model2.isLocal then
                        { model2
                            | otherPlayerid =
                                case playerid of
                                    Just p ->
                                        p

                                    Nothing ->
                                        ""
                        }

                    else
                        case playerid of
                            Nothing ->
                                model2

                            Just pid ->
                                { model2 | playerid = pid }
            in
            model3 |> withNoCmd

        LeaveRsp { gameid, player } ->
            let
                model2 =
                    { model
                        | gameid = ""
                        , playerid = ""
                        , otherPlayerid = ""
                        , error =
                            if player == model.chooseFirst then
                                Nothing

                            else
                                Just "Other player left"
                    }
            in
            if model.isLocal then
                model2 |> withNoCmd

            else
                { model2 | isLive = False }
                    |> withCmd
                        (Cmd.none
                         --WebSocket.makeClose model.serverUrl
                         --|> webSocketSend
                        )

        UpdateRsp { gameid, gameState } ->
            { model
                | gameState = gameState
                , decoration = NoDecoration
                , firstSelection = NoDecoration
            }
                |> withNoCmd

        PlayRsp { gameid, gameState, decoration } ->
            if not model.isLocal then
                { model
                    | gameState = gameState
                    , decoration = decoration
                }
                    |> withNoCmd

            else
                let
                    ( newDecoration, firstSelection ) =
                        case decoration of
                            AlreadyFilledDecoration _ ->
                                ( decoration, model.firstSelection )

                            _ ->
                                ( NoDecoration, decoration )
                in
                { model
                    | gameState = gameState
                    , decoration = newDecoration
                    , firstSelection = firstSelection
                }
                    |> withNoCmd

        ResignRsp { gameid, gameState, player } ->
            { model
                | gameState = gameState
                , decoration = NoDecoration
                , firstSelection = NoDecoration
                , error =
                    if model.isLocal then
                        Nothing

                    else if model.chooseFirst == player then
                        Just "You resigned."

                    else
                        Just <| playerName player model ++ " resigned."
            }
                |> withNoCmd

        AnotherGameRsp { gameid, gameState, player } ->
            { model
                | gameState = gameState
                , decoration = NoDecoration
                , firstSelection = NoDecoration
                , player = player
                , error =
                    if not model.isLocal && player /= model.chooseFirst then
                        Just "Other player asked for a new game"

                    else
                        Nothing
            }
                |> withNoCmd

        GameOverRsp { gameid, gameState } ->
            { model
                | gameState = gameState
                , decoration = NoDecoration
                , firstSelection = NoDecoration
            }
                |> withNoCmd

        PublicGamesRsp { games } ->
            { model | publicGames = games }
                |> withNoCmd

        PublicGamesUpdateRsp { added, removed } ->
            let
                games =
                    List.filter
                        (\{ gameid } -> not <| List.member gameid removed)
                        model.publicGames
            in
            { model | publicGames = List.concat [ games, added ] }
                |> withNoCmd

        ErrorRsp { request, text } ->
            let
                model2 =
                    { model
                        | error = Just text
                        , connectionReason = NoConnection
                        , page =
                            if model.page == PublicPage then
                                MainPage

                            else
                                model.page
                        , publicGames = []
                    }

                model3 =
                    if model.isLocal then
                        model2

                    else
                        { model2
                            | isLive = False
                            , playerid = ""
                            , gameid = ""
                        }
            in
            model3
                |> withCmd
                    (if
                        not model2.isLocal
                            && (model.connectionReason /= NoConnection)
                     then
                        Cmd.none
                        --WebSocket.makeClose model2.serverUrl
                        --  |> webSocketSend

                     else
                        Cmd.none
                    )

        ChatRsp { gameid, name, text } ->
            let
                ( chatSettings, cmd ) =
                    ElmChat.addLineSpec model.chatSettings <|
                        ElmChat.makeLineSpec text
                            (Just name)
                            --(Just model.time)
                            Nothing
            in
            { model | chatSettings = chatSettings }
                |> withCmds
                    [ cmd

                    -- Kluge. ElmChat is supposed to do this
                    , Task.attempt (\_ -> Noop) <|
                        Dom.setViewportOf ids.chatOutput 0 1000000
                    ]

        _ ->
            model |> withNoCmd


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            { mdl | funnelState = state }
    in
    case response of
        ErrorResponse error ->
            case error of
                WebSocket.SocketAlreadyOpenError _ ->
                    socketHandler
                        (ConnectedResponse { key = "", description = "" })
                        state
                        model

                _ ->
                    { model | error = Just <| WebSocket.errorToString error }
                        |> withNoCmd

        WebSocket.MessageReceivedResponse received ->
            let
                string =
                    received.message
            in
            case WSFED.decodeMessage ED.messageDecoder string of
                Err errmsg ->
                    { model | error = Just errmsg }
                        |> withNoCmd

                Ok message ->
                    { model | error = Nothing }
                        |> withCmd
                            (Task.perform (IncomingMessage model.interface) <|
                                Task.succeed message
                            )

        ClosedResponse { expected, reason } ->
            { model
                | isLive = False
                , connectionReason = NoConnection
                , error =
                    if Debug.log "ClosedResponse, expected" expected then
                        model.error

                    else
                        Just <| "Connection unexpectedly closed: " ++ reason
            }
                |> withNoCmd

        ConnectedResponse _ ->
            { model | error = Nothing }
                |> withCmd
                    (case model.connectionReason of
                        NoConnection ->
                            Cmd.none

                        StartGameConnection ->
                            let
                                settings =
                                    model.settings
                            in
                            send model <|
                                NewReq
                                    { name = model.settings.name
                                    , player = model.chooseFirst
                                    , publicType =
                                        if not settings.isPublic then
                                            NotPublic

                                        else
                                            case settings.forName of
                                                "" ->
                                                    EntirelyPublic

                                                forName ->
                                                    PublicFor forName
                                    , restoreState = Nothing
                                    }

                        JoinGameConnection ->
                            send model <|
                                JoinReq
                                    { gameid = model.gameid
                                    , name = model.settings.name
                                    }

                        PublicGamesConnection ->
                            send model <|
                                PublicGamesReq
                                    { subscribe = model.page == PublicPage
                                    , forName = model.settings.name
                                    , gameid = Just model.gameid
                                    }

                        UpdateConnection ->
                            send model <|
                                UpdateReq
                                    { playerid = model.playerid }
                    )

        _ ->
            model |> withNoCmd


focusId : String -> Cmd Msg
focusId id =
    Task.attempt (\_ -> Noop) (Dom.focus id)


onKeydown : (Int -> msg) -> Attribute msg
onKeydown tagger =
    on "keydown" (JD.map tagger keyCode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( mdl, cmd ) =
            updateInternal msg model

        { zephyrus, notus } =
            mdl.gameState.players

        focus =
            --not mdl.isLocal && mdl.isLive && zephyrus /= "" && notus /= ""
            --might be able to be smart and do this just on desktop, but not for now
            False

        doSave =
            case msg of
                Noop ->
                    False

                Click _ ->
                    cmd == Cmd.none

                NewGame ->
                    False

                Process _ ->
                    False

                IncomingMessage _ _ ->
                    cmd == Cmd.none

                ClearStorage ->
                    False

                ChatUpdate _ _ ->
                    False

                ChatSend _ _ ->
                    False

                ChatClear ->
                    False

                DelayedAction _ _ ->
                    False

                _ ->
                    True
    in
    mdl
        |> withCmds
            [ cmd
            , if focus && doSave then
                focusId ids.chatInput

              else
                Cmd.none
            , if model.started && doSave then
                putModel mdl

              else
                Cmd.none
            ]


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    let
        gameState =
            model.gameState

        settings =
            model.settings
    in
    case msg of
        Noop ->
            model |> withNoCmd

        IncomingMessage interface message ->
            incomingMessage interface message model

        SetDecoration decoration ->
            { model | decoration = decoration }
                |> withNoCmd

        SetChooseFirst player ->
            { model | chooseFirst = player }
                |> withNoCmd

        SetIsLocal isLocal ->
            let
                model2 =
                    { model
                        | isLocal = isLocal
                        , isLive = False
                        , gameid = ""
                        , playerid = ""
                        , otherPlayerid = ""
                        , interface =
                            if isLocal then
                                proxyServer

                            else
                                model.interface
                    }
            in
            model2
                |> withCmd
                    (if isLocal && not model.isLocal then
                        Cmd.batch
                            [ if model.isLive then
                                send model <|
                                    LeaveReq { playerid = model.playerid }

                              else
                                Cmd.none
                            , send model2 <| NewReq initialNewReqBody
                            ]

                     else
                        Cmd.none
                    )

        SetName name ->
            { model | settings = { settings | name = name } }
                |> withNoCmd

        SetIsPublic isPublic ->
            { model | settings = { settings | isPublic = isPublic } }
                |> withCmd
                    (if isPublic && not settings.isPublic then
                        focusId ids.forName

                     else
                        Cmd.none
                    )

        SetForName forName ->
            { model | settings = { settings | forName = forName } }
                |> withNoCmd

        SetServerUrl serverUrl ->
            { model | serverUrl = serverUrl }
                |> withNoCmd

        SetGameid gameid ->
            { model | gameid = gameid }
                |> withNoCmd

        SetPage page ->
            let
                getGames =
                    page == PublicPage || model.page == PublicPage
            in
            { model | page = page }
                |> webSocketConnect PublicGamesConnection

        SetHideTitle hideTitle ->
            { model | settings = { settings | hideTitle = hideTitle } }
                |> withNoCmd

        ResetScore ->
            if not <| model.isLocal then
                model |> withNoCmd

            else
                case model.interface of
                    ServerInterface si ->
                        case si.state of
                            Nothing ->
                                model |> withNoCmd

                            Just s ->
                                case si.state of
                                    Nothing ->
                                        model |> withNoCmd

                                    Just state ->
                                        let
                                            gs =
                                                { gameState
                                                    | score = Types.zeroScore
                                                }
                                        in
                                        { model
                                            | gameState = gs
                                            , interface =
                                                ServerInterface
                                                    { si
                                                        | state =
                                                            Just <|
                                                                ServerInterface.updateGame
                                                                    model.gameid
                                                                    gs
                                                                    state
                                                    }
                                        }
                                            |> withNoCmd

        NewGame ->
            let
                resigning =
                    if not model.isLocal then
                        model.chooseFirst

                    else
                        gameState.whoseTurn

                pid =
                    if not model.isLocal then
                        model.playerid

                    else
                        case resigning of
                            Zephyrus ->
                                model.playerid

                            Notus ->
                                model.otherPlayerid

                ( playerid, placement ) =
                    if gameState.winner == NoWinner then
                        ( pid
                        , ChooseResign resigning
                        )

                    else
                        let
                            player =
                                if model.isLocal then
                                    Zephyrus

                                else
                                    model.chooseFirst
                        in
                        ( model.playerid, ChooseNew player )
            in
            model
                |> withCmd
                    (send model <|
                        PlayReq
                            { playerid = playerid
                            , placement = placement
                            }
                    )

        StartGame ->
            startGame model

        Join ->
            join model

        JoinGame gameid ->
            join { model | gameid = gameid }

        Disconnect ->
            disconnect model

        ClearStorage ->
            let
                ( mdl, cmd ) =
                    init JE.null "url" model.key
            in
            { mdl | started = True }
                |> withCmds [ clear, cmd ]

        Click ( row, col ) ->
            if gameState.winner /= NoWinner || (not <| isPlaying model) then
                model |> withNoCmd

            else
                doClick row col model

        ChatUpdate chatSettings cmd ->
            { model | chatSettings = chatSettings }
                |> withCmd (putChat chatSettings)

        ChatSend line chatSettings ->
            chatSend line chatSettings model

        ChatClear ->
            let
                chatSettings =
                    model.chatSettings

                newSettings =
                    { chatSettings | lines = [] }
            in
            { model | chatSettings = newSettings }
                |> withCmd (putChat newSettings)

        DelayedAction updater time ->
            updater { model | time = time }

        SetZone zone ->
            let
                chatSettings =
                    model.chatSettings
            in
            { model | chatSettings = { chatSettings | zone = zone } }
                |> withNoCmd

        SetGameCount string ->
            let
                simulatorState =
                    model.simulatorState
            in
            { model
                | simulatorState =
                    { simulatorState
                        | gameCount =
                            String.toInt string
                                |> Maybe.withDefault simulatorState.gameCount
                    }
            }
                |> withNoCmd

        ToggleSimulator ->
            let
                simulatorState =
                    model.simulatorState

                gamesLeft =
                    if simulatorState.gamesLeft == 0 then
                        simulatorState.gameCount

                    else
                        0
            in
            { model
                | simulatorState =
                    { simulatorState
                        | gamesLeft = gamesLeft
                        , simulatorResult = SimulatorResult 0 0 0 0
                    }
            }
                |> withCmd
                    (if gamesLeft > 0 then
                        simulatorStepCmd ()

                     else
                        Cmd.none
                    )

        SimulatorStep ->
            let
                simulatorState =
                    model.simulatorState

                count =
                    simulatorState.gamesLeft

                loop seed left c1 c2 s1 s2 =
                    if left <= 0 then
                        ( SimulatorResult c1 c2 s1 s2, seed )

                    else
                        let
                            ( winner, score, seed2 ) =
                                Board.simulateGame seed

                            ( ( c12, c22 ), ( s12, s22 ) ) =
                                case winner of
                                    ZephyrusWinner ->
                                        ( ( c1 + 1, c2 ), ( s1 + score, s2 ) )

                                    NotusWinner ->
                                        ( ( c1, c2 + 1 ), ( s1, s2 + score ) )

                                    _ ->
                                        ( ( c1, c2 ), ( s2, s2 ) )
                        in
                        loop seed2 (left - 1) c12 c22 s12 s22

                gamesLeft =
                    simulatorState.gamesLeft - count

                oldResult =
                    simulatorState.simulatorResult

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
                | simulatorState =
                    { simulatorState
                        | simulatorResult = simulatorResult
                        , gamesLeft = gamesLeft
                    }
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

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    -- Maybe we should display an error here,
                    -- but I don't think it will ever happen.
                    model |> withNoCmd

                Ok res ->
                    res


chatSend : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSend line chatSettings model =
    model
        |> withCmd (delayedAction <| chatSendInternal line chatSettings)


chatSendInternal : String -> ChatSettings -> Model -> ( Model, Cmd Msg )
chatSendInternal line chatSettings model =
    { model | chatSettings = chatSettings }
        |> withCmd
            (send model <|
                ChatReq
                    { playerid = model.playerid
                    , text = line
                    }
            )


delayedAction : (Model -> ( Model, Cmd Msg )) -> Cmd Msg
delayedAction updater =
    Task.perform (DelayedAction updater) Time.now


makeWebSocketServer : Model -> ServerInterface
makeWebSocketServer model =
    WebSocketFramework.makeServer
        (getCmdPort WebSocket.moduleName ())
        ED.messageEncoder
        model.serverUrl
        Noop


webSocketConnect : ConnectionReason -> Model -> ( Model, Cmd Msg )
webSocketConnect reason model =
    if model.isLocal then
        { model
            | interface = proxyServer
            , isLive = True
        }
            |> withNoCmd

    else
        { model
            | interface = makeWebSocketServer model
            , connectionReason = reason
        }
            |> withCmd
                (WebSocket.makeOpen model.serverUrl
                    |> webSocketSend
                )


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    webSocketConnect StartGameConnection model


join : Model -> ( Model, Cmd Msg )
join model =
    webSocketConnect JoinGameConnection model


disconnect : Model -> ( Model, Cmd Msg )
disconnect model =
    { model | isLive = False }
        |> withCmd
            (if model.isLive && not model.isLocal then
                send model <|
                    LeaveReq { playerid = model.playerid }

             else
                Cmd.none
            )


send : Model -> Message -> Cmd Msg
send model message =
    ServerInterface.send model.interface <| Debug.log "send" message


simulatorStepCmd : () -> Cmd Msg
simulatorStepCmd x =
    Task.perform (\_ -> SimulatorStep) <| Task.succeed x


doClick : Int -> Int -> Model -> ( Model, Cmd Msg )
doClick row col model =
    let
        gameState =
            model.gameState

        withPlayReq playerid placement =
            withCmd <|
                send model
                    (PlayReq
                        { playerid = playerid
                        , placement = placement
                        }
                    )
    in
    if not model.isLocal then
        let
            withACmd =
                withPlayReq model.playerid <|
                    case model.chooseFirst of
                        Zephyrus ->
                            ChooseCol col

                        Notus ->
                            ChooseRow row
        in
        case model.decoration of
            AlreadyFilledDecoration _ ->
                if model.chooseFirst /= gameState.whoseTurn then
                    model |> withNoCmd

                else
                    model |> withACmd

            _ ->
                model |> withACmd

    else
        case model.firstSelection of
            ColSelectedDecoration selectedCol ->
                case model.decoration of
                    RowSelectedDecoration selectedRow ->
                        if row /= selectedRow then
                            { model
                                | decoration =
                                    RowSelectedDecoration row
                            }
                                |> withNoCmd

                        else
                            model
                                |> (withPlayReq model.otherPlayerid <| ChooseRow row)

                    AlreadyFilledDecoration ( ar, ac ) ->
                        model
                            |> (case gameState.whoseTurn of
                                    Zephyrus ->
                                        withPlayReq model.playerid <| ChooseCol col

                                    Notus ->
                                        withPlayReq model.otherPlayerid <| ChooseRow row
                               )

                    _ ->
                        { model
                            | decoration =
                                RowSelectedDecoration row
                        }
                            |> withNoCmd

            RowSelectedDecoration selectedRow ->
                case model.decoration of
                    ColSelectedDecoration selectedCol ->
                        if col /= selectedCol then
                            { model
                                | decoration =
                                    ColSelectedDecoration col
                            }
                                |> withNoCmd

                        else
                            model
                                |> (withPlayReq model.playerid <| ChooseCol col)

                    AlreadyFilledDecoration ( ar, ac ) ->
                        model
                            |> (case gameState.whoseTurn of
                                    Zephyrus ->
                                        withPlayReq model.playerid <| ChooseCol col

                                    Notus ->
                                        withPlayReq model.otherPlayerid <| ChooseRow row
                               )

                    _ ->
                        { model
                            | decoration =
                                ColSelectedDecoration col
                        }
                            |> withNoCmd

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
                            |> withNoCmd

                    ColSelectedDecoration c ->
                        if c == col then
                            model
                                |> (withPlayReq model.playerid <| ChooseCol col)

                        else
                            { model
                                | decoration =
                                    ColSelectedDecoration col
                            }
                                |> withNoCmd

                    RowSelectedDecoration r ->
                        if r == row then
                            model
                                |> (withPlayReq model.otherPlayerid <| ChooseRow row)

                        else
                            { model
                                | decoration =
                                    RowSelectedDecoration row
                            }
                                |> withNoCmd

                    _ ->
                        model |> withNoCmd


cellName : ( Int, Int ) -> String
cellName ( rowidx, colidx ) =
    Board.colToString colidx ++ Board.rowToString rowidx


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onResize WindowResize
        , PortFunnels.subscriptions Process model
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
    min (90 * w) (65 * h) // 100


herculanumStyle : Attribute msg
herculanumStyle =
    style "font-family" "Herculanum, sans-serif"


view : Model -> Document Msg
view model =
    let
        bsize =
            boardSize model

        settings =
            model.settings
    in
    { title = "ZEPHYRNOT"
    , body =
        [ if bsize == 0 then
            text ""

          else
            div []
                [ if settings.hideTitle then
                    text ""

                  else
                    div
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

                    PublicPage ->
                        publicPage bsize model
                ]
        ]
    }


ids =
    { chatOutput = "chatOutput"
    , chatInput = "chatInput"
    , forName = "forName"
    }


mainPage : Int -> Model -> Html Msg
mainPage bsize model =
    let
        settings =
            model.settings

        gameState =
            model.gameState

        score =
            gameState.score

        count =
            Board.count gameState.board

        { zephyrus, notus } =
            gameState.players

        ( playing, message ) =
            if not model.isLive then
                ( False
                , "Enter \"Your Name\" and either click \"Start Game\" or enter \"Game ID\" and click \"Join\""
                )

            else if zephyrus == "" || notus == "" then
                ( False
                , let
                    waitingFor =
                        if zephyrus == "" then
                            "Zephyrus"

                        else
                            "Notus"
                  in
                  "Waiting for " ++ waitingFor ++ " to join"
                )

            else
                ( True
                , let
                    winString player =
                        let
                            rawName =
                                playerName player model

                            name =
                                if model.isLocal || player /= model.chooseFirst then
                                    rawName

                                else
                                    "You (" ++ rawName ++ ")"
                        in
                        name ++ " won in " ++ String.fromInt count ++ "!"
                  in
                  case gameState.winner of
                    ZephyrusWinner ->
                        winString Zephyrus

                    NotusWinner ->
                        winString Notus

                    NoWinner ->
                        case model.firstSelection of
                            NoDecoration ->
                                case model.decoration of
                                    NoDecoration ->
                                        if model.chooseFirst == Zephyrus then
                                            "Zephyrus pick a column"

                                        else
                                            "Notus pick a row"

                                    ColSelectedDecoration _ ->
                                        if model.isLocal then
                                            "Zephyrus confirm or pick another column"

                                        else
                                            "Waiting for Notus to pick a row (you may pick another column)"

                                    RowSelectedDecoration _ ->
                                        if model.isLocal then
                                            "Notus confirm or pick another row"

                                        else
                                            "Waiting for Zephyrus to pick a column (you may pick another row)"

                                    AlreadyFilledDecoration _ ->
                                        case gameState.whoseTurn of
                                            Zephyrus ->
                                                if
                                                    model.isLocal
                                                        || (model.chooseFirst == Zephyrus)
                                                then
                                                    "Zephyrus pick another column"

                                                else
                                                    "Waiting for Zephyrus to pick another column"

                                            Notus ->
                                                if
                                                    model.isLocal
                                                        || (model.chooseFirst == Notus)
                                                then
                                                    "Notus pick another row"

                                                else
                                                    "Waiting for Notus to pick another row"

                            RowSelectedDecoration _ ->
                                case model.decoration of
                                    NoDecoration ->
                                        "Notus chose. Zephyrus pick a column"

                                    AlreadyFilledDecoration _ ->
                                        case gameState.whoseTurn of
                                            Zephyrus ->
                                                if
                                                    model.isLocal
                                                        || (model.chooseFirst == Zephyrus)
                                                then
                                                    "Zephyrus pick another column"

                                                else
                                                    "Waiting for Zephyrus to pick another column"

                                            Notus ->
                                                if
                                                    model.isLocal
                                                        || (model.chooseFirst == Notus)
                                                then
                                                    "Notus pick another row"

                                                else
                                                    "Waiting for Notus to pick another row"

                                    _ ->
                                        "Notus chose. Zephyrus confirm or pick another column"

                            ColSelectedDecoration _ ->
                                case model.decoration of
                                    NoDecoration ->
                                        "Zephyrus chose. Notus pick a row"

                                    AlreadyFilledDecoration _ ->
                                        case gameState.whoseTurn of
                                            Zephyrus ->
                                                "Zephyrus pick another column"

                                            Notus ->
                                                "Notus pick another row"

                                    _ ->
                                        "Zephyrus chose. Notus confirm or pick another row"

                            AlreadyFilledDecoration _ ->
                                -- Can't happen
                                ""
                )
    in
    div [ align "center" ]
        [ Board.render bsize
            Click
            (Just <| Board.getSizer DefaultSizer)
            model.decoration
            gameState.path
            gameState.board
        , span
            []
            [ br
            , case model.error of
                Nothing ->
                    text ""

                Just err ->
                    span [ style "color" "red" ]
                        [ text err
                        , br
                        ]
            , span
                [ style "color"
                    (if not playing || gameState.winner == NoWinner then
                        "green"

                     else
                        "orange"
                    )
                , style "font-weight"
                    (if gameState.winner == NoWinner then
                        "normal"

                     else
                        "bold"
                    )
                ]
                [ text message ]
            , br
            , if gameState.winner /= NoWinner then
                text ""

              else
                span []
                    [ b "Stone Placer: "
                    , text <|
                        case gameState.whoseTurn of
                            Zephyrus ->
                                "Zephyrus"

                            Notus ->
                                "Notus"
                    , br
                    ]
            , if not model.isLocal && model.isLive then
                span []
                    [ if zephyrus == "" || notus == "" then
                        text ""

                      else
                        span []
                            [ ElmChat.styledInputBox [ id ids.chatInput ]
                                []
                                --width in chars
                                40
                                --id
                                "Send"
                                ChatSend
                                model.chatSettings
                            , text " "
                            , button [ onClick ChatClear ]
                                [ text "Clear" ]
                            , ElmChat.chat model.chatSettings
                            , br
                            ]
                    , b "Zephyrus: "
                    , text <|
                        case zephyrus of
                            "" ->
                                ""

                            _ ->
                                if model.chooseFirst == Zephyrus then
                                    "You (" ++ zephyrus ++ ")"

                                else
                                    zephyrus
                    , br
                    , b "Notus: "
                    , text <|
                        case notus of
                            "" ->
                                ""

                            _ ->
                                if model.chooseFirst == Notus then
                                    "You (" ++ notus ++ ")"

                                else
                                    notus
                    ]

              else
                let
                    disabled =
                        model.isLive && not model.isLocal
                in
                span []
                    [ b <|
                        if model.isLocal then
                            "Choose first: "

                        else
                            "You play: "
                    , radio "choose"
                        "Zephyrus"
                        (model.chooseFirst == Zephyrus)
                        disabled
                        (SetChooseFirst Zephyrus)
                    , text " "
                    , radio "choose"
                        "Notus"
                        (model.chooseFirst == Notus)
                        disabled
                        (SetChooseFirst Notus)
                    ]
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
            , if not model.isLocal then
                text ""

              else
                button [ onClick ResetScore ]
                    [ text "Reset" ]
            , br
            , b "Local: "
            , input
                [ type_ "checkbox"
                , checked model.isLocal
                , onCheck SetIsLocal
                , disabled <| not model.isLocal && model.isLive
                ]
                []
            , text " "
            , button
                [ onClick NewGame
                , disabled (not <| isPlaying model)
                ]
                [ text <|
                    if gameState.winner == NoWinner then
                        "Resign"

                    else
                        "New Game"
                ]
            , if model.isLocal then
                text ""

              else
                div [ align "center" ]
                    [ if model.isLive then
                        div [ align "center" ]
                            [ b "Game ID: "
                            , text model.gameid
                            , br
                            , button
                                [ onClick Disconnect ]
                                [ text "Disconnect" ]
                            ]

                      else
                        div [ align "center" ]
                            [ b "Your Name: "
                            , input
                                [ onInput SetName
                                , value settings.name
                                , size 20
                                ]
                                []
                            , br

                            {-
                               , b "Server: "
                               , input
                                   [ onInput SetServerUrl
                                   , value model.serverUrl
                                   , size 40
                                   , disabled True
                                   ]
                                   []
                               , text " "
                            -}
                            , b "Public: "
                            , input
                                [ type_ "checkbox"
                                , checked settings.isPublic
                                , onCheck SetIsPublic
                                ]
                                []
                            , if not settings.isPublic then
                                text ""

                              else
                                span []
                                    [ b " for name: "
                                    , input
                                        [ onInput SetForName
                                        , value settings.forName
                                        , size 20
                                        , id ids.forName
                                        ]
                                        []
                                    ]
                            , text " "
                            , button
                                [ onClick StartGame
                                , disabled <| settings.name == ""
                                ]
                                [ text "Start Game" ]
                            , br
                            , b "Game ID: "
                            , input
                                [ onInput SetGameid
                                , value model.gameid
                                , size 16
                                , onKeydown
                                    (\code ->
                                        if code == 13 then
                                            Join

                                        else
                                            Noop
                                    )
                                ]
                                []
                            , text " "
                            , button
                                [ onClick Join
                                , disabled <|
                                    (settings.name == "")
                                        || (model.gameid == "")
                                ]
                                [ text "Join"
                                ]
                            ]
                    ]
            ]
        , p []
            [ text "Moves: "
            , text <| movesToString gameState.moves
            ]
        , p []
            [ a
                [ href "#"
                , onClick <| SetPage PublicPage
                ]
                [ text "Public" ]
            , text " "
            , a
                [ href "#"
                , onClick <| SetPage AuxPage
                ]
                [ text "Aux" ]
            , text " "
            , a
                [ href "#"
                , onClick <| SetPage InstructionsPage
                ]
                [ text "Instructions" ]
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
            , br
            , button
                [ onClick ClearStorage ]
                [ text <| "Clear" ]
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
        [ radio "decoration" "none" none False (SetDecoration NoDecoration)
        , text " "
        , radio "decoration" "row" rowp False (SetDecoration (RowSelectedDecoration 3))
        , text " "
        , radio "decoration" "col" colp False (SetDecoration (ColSelectedDecoration 2))
        , text " "
        , radio "decoration"
            "conflict"
            filledp
            False
            (SetDecoration (AlreadyFilledDecoration ( 1, 3 )))
        ]


radio : String -> String -> Bool -> Bool -> msg -> Html msg
radio group name isChecked isDisabled msg =
    label []
        [ input
            [ type_ "radio"
            , Attributes.name group
            , onClick msg
            , checked isChecked
            , disabled isDisabled
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
Play may either be networked, through the server at zephyrus.com, or
local, with no server connection. The "Local" checkbox controls this.

**Networked Play**

The "You play" radio buttons control which player you will be if you
click "Start Game". Zephyrus is the first stone placer.

Fill in "Your Name" and either click "Start Game" or fill in the
"Game ID" and click "Join". If you "Start Game", the "Game ID" will be
filled in, and you'll need to give this to the other player, so they
can "Join" (or use a public game).

To create a public game, which will appear on the "Public" page, check
the "Public" checkbox. If you fill in "for name", then only players
with that name will be able to see that game. Otherwise, anyone who
goes to the public games page will be able to join your game.

After both players are connected, on each turn, Zephyrus clicks a
column and Notus clicks a row. Instructions continue at "Both Local
and Networked" below.

**Local Play**

The "Choose first" radio buttons control which player chooses a row or
column first for each stone placement. This is amenable to using a
portable device (e.g. a smart phone or tablet), and passing it back
and forth to make selections.

The "Reset" button resets the score numbers to 0.

The players must choose who will be Zephyrus, placing the first stone,
and attempting to create a path connecting the east and west
edges, and who will be Notus, placing the second stone, and attempting
to create a path connecting the north and south edges.

Zephyrus taps a column to select it, then taps another column to
change the selection, or the same column again to choose it.

Notus taps a row to select it, then taps another row to
change the selection, or the same column again to choose it.

**Both Local and Networked**

After both players have made their selection, the stone at the
intersection of the selected row and column is placed. If there is
already a stone there, it is highlighted in red, and the player
placing the stone must choose another row or column that is
unoccupied.

Play continues until there is a path connecting two opposite
edges. Click the "New Game" button to play again.

Click the "Resign" button to let the Wookie win. Click the
"Disconnect" button to stop playing, leaving the other player in the
lurch.

Click the "Clear" button at the bottom of the page to remove all stored
state from your browser, and restart with an empty board.

The "Public" games page lists all public games known to the server,
except those that were registered for another name than "Your
Name". Click on an underlined GameId to join that game. The "Creator"
column shows the name of the player who started the game. The "Player"
column shows which player he will be. You will be the other player.

The "Aux" page has a "Hide Title" checkbox, which, if checked, hides
the title at the top of the page, making more fit in your browser
without scrolling. The "Simulate" button will run the number of games
in the "Game Count" text box, using random moves, and display the
outcome.

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
        simulatorState =
            model.simulatorState

        { horizontalWins, verticalWins, horizontalScore, verticalScore } =
            simulatorState.simulatorResult

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

        settings =
            model.settings
    in
    rulesDiv False
        [ br
        , playButton
        , rulesDiv True
            [ br, b "Aux" ]
        , p [ align "center" ]
            [ b "Hide Title: "
            , input
                [ type_ "checkbox"
                , checked settings.hideTitle
                , onCheck SetHideTitle
                ]
                []
            , br
            , b "Game Count: "
            , input
                [ onInput SetGameCount
                , value simulatorState.gameCountString
                , size 4
                ]
                []
            , text " "
            , button
                [ onClick ToggleSimulator ]
                [ if simulatorState.gamesLeft > 0 then
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


th : String -> Html Msg
th string =
    Html.th [] [ text string ]


publicPage : Int -> Model -> Html Msg
publicPage bsize model =
    let
        settings =
            model.settings
    in
    rulesDiv False
        [ br
        , playButton
        , rulesDiv True
            [ br, b "Public Games" ]
        , p [ align "center" ]
            [ if isPlaying model then
                p [ style "color" "red" ]
                    [ text "You're playing a game. What are you doing here?" ]

              else
                text ""
            , table [ class "prettytable" ] <|
                List.concat
                    [ [ tr []
                            [ th "GameId"
                            , th "Creator"
                            , th "Player"
                            , th "For you"
                            ]
                      ]
                    , List.map
                        (renderPublicGameRow model.gameid
                            settings.name
                            (isPlaying model)
                        )
                        model.publicGames
                    ]
            ]
        , playButton
        ]


renderPublicGameRow : String -> String -> Bool -> PublicGame -> Html Msg
renderPublicGameRow myGameid name playing { gameid, creator, player, forName } =
    let
        center =
            style "text-align" "center"
    in
    tr []
        [ td [ center ]
            [ if gameid == myGameid || playing then
                text gameid

              else
                a
                    [ href "#"
                    , onClick <| JoinGame gameid
                    ]
                    [ text gameid ]
            ]
        , td [ center ]
            [ if gameid == myGameid then
                text <| "You (" ++ creator ++ ")"

              else
                text creator
            ]
        , td [ center ] [ text <| playerString player ]
        , td [ center ]
            [ if myGameid == gameid then
                text <| Maybe.withDefault "" forName

              else
                input
                    [ type_ "checkbox"
                    , checked <| Interface.forNameMatches name forName
                    ]
                    []
            ]
        ]


playerString : Player -> String
playerString player =
    case player of
        Zephyrus ->
            "Zephyrus"

        Notus ->
            "Notus"


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

        playerid =
            model.playerid
    in
    put pk.model <| Just value


putChat : ChatSettings -> Cmd Msg
putChat settings =
    ElmChat.settingsEncoder settings
        |> Just
        |> put pk.chat


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put key value)


get : String -> Cmd Msg
get key =
    localStorageSend (LocalStorage.get key)


clear : Cmd Msg
clear =
    localStorageSend (LocalStorage.clear "")


localStoragePrefix : String
localStoragePrefix =
    "zephyrnot"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


webSocketSend : WebSocket.Message -> Cmd Msg
webSocketSend message =
    WebSocket.send (getCmdPort WebSocket.moduleName ()) <|
        Debug.log "webSocketSend" message


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort Process moduleName False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        ]
        getCmdPort


{-| Persistent storage keys
-}
pk =
    { model = "model"
    , chat = "chat"
    }
