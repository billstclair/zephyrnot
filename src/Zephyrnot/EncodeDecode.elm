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


module Zephyrnot.EncodeDecode exposing (decodeSavedModel)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Zephyrnot.Types exposing (SavedModel)


decodeSavedModel : Value -> Result String SavedModel
decodeSavedModel value =
    Err "TODO"
