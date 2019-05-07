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
import Html.Attributes
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
import Zephyrnot.Board as Board exposing (Board, Decoration(..), Winner(..))


type alias Model =
    { key : Key
    , windowSize : ( Int, Int )
    , decoration : Decoration
    , board : Board
    }


type Msg
    = Noop
    | SetDecoration Decoration
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


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { key = key
    , windowSize = ( 1024, 768 )
    , decoration = ColSelectedDecoration 3
    , board =
        Board.empty
            -- Temporary
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

        Click ( row, col ) ->
            let
                ( r, c ) =
                    Debug.log "Click" ( row, col )
            in
            model |> withNoCmd

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
            , let
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
                [ radio "none" none (SetDecoration NoDecoration)
                , text " "
                , radio "row" rowp (SetDecoration (RowSelectedDecoration 3))
                , text " "
                , radio "col" colp (SetDecoration (ColSelectedDecoration 2))
                , text " "
                , radio "conflict"
                    filledp
                    (SetDecoration (AlreadyFilledDecoration ( 1, 3 )))
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


radio : String -> Bool -> msg -> Html msg
radio name isChecked msg =
    label []
        [ input
            [ type_ "radio"
            , onClick msg
            , checked isChecked
            ]
            []
        , text name
        ]
