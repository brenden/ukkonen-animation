module UkkonenVisualization (..) where

import Html exposing (pre, text)
import Html.Attributes exposing (class)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (..)
import Json.Encode as Json
import Window
import UkkonenTree exposing (..)
import UkkonenAlgorithm exposing (..)


port tree : Signal Json.Value
port tree =
    Signal.map (\content -> UkkonenAlgorithm.buildTree content.string |> toJson) inputString.signal


inputString : Signal.Mailbox Content
inputString =
    Signal.mailbox noContent


main : Signal Element
main =
    Signal.map (field defaultStyle (Signal.message inputString.address) "Input string") inputString.signal
