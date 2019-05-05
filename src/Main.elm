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
import Url exposing (Url)


type alias Model =
    { key : Key
    }


type Msg
    = Noop
    | HandleUrlRequest UrlRequest
    | HandleUrlChange Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = HandleUrlRequest
        , onUrlChange = HandleUrlChange
        }


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { key = key } |> withNoCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model |> withNoCmd

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


br : Html Msg
br =
    Html.br [] []


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
                [ text "Board goes here" ]
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
