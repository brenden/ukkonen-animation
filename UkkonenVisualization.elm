module UkkonenVisualization (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (..)
import Json.Encode as Json
import Window
import UkkonenTree exposing (..)
import UkkonenAlgorithm exposing (..)


defaultInput = "abcabxabzxz"

port tree : Signal Json.Value
port tree =
    Signal.map (\content -> UkkonenAlgorithm.buildTree content.string |> toJson) inputString.signal


inputString : Signal.Mailbox Content
inputString =
    Signal.mailbox noContent


inputField : Content -> Element
inputField = field defaultStyle (Signal.message inputString.address) defaultInput


main : Signal Html
main =
    Signal.map view inputString.signal


view : Content -> Html
view content = section [ id "visualization" ]
    [
      h1 [] [text "Visualization of Ukkonen's Algorithm"],
      inputField content |> fromElement
    ]
