module UkkonenVisualization (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Text
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (..)
import Json.Encode as Json
import Window
import UkkonenTree exposing (..)
import UkkonenAlgorithm exposing (..)


defaultInput =
    "abcabxabzxz"


port tree : Signal Json.Value
port tree =
    Signal.map (\content -> UkkonenAlgorithm.buildTree content.string |> toJson) inputString.signal


inputString : Signal.Mailbox Content
inputString =
    Signal.mailbox noContent


inputFieldStyle : Style
inputFieldStyle =
    let
        textDefaultStyle = Text.defaultStyle
    in
        { defaultStyle
            | padding = uniformly 1
            , outline = { color = rgb 255 0 0, width = uniformly 2, radius = 4 }
            , style = { textDefaultStyle | height = Just 15, color = rgb 123 53 20 }
        }


inputField : Content -> Element
inputField =
    field inputFieldStyle (Signal.message inputString.address) defaultInput


main : Signal Html
main =
    Signal.map view inputString.signal


view : Content -> Html
view content =
    section
        [ id "visualization" ]
        [ h1 [] [ text "Visualization of Ukkonen's Algorithm" ]
        , inputField content |> fromElement
        ]
