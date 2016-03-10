module UkkonenVisualization (..) where

import Html exposing (..)
import Html.Attributes exposing (id, class)
import Text
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Graphics.Input.Field exposing (..)
import Json.Encode as Json
import Window
import UkkonenTree exposing (..)
import UkkonenAlgorithm exposing (..)


baseColor = rgb 57 75 169
lightGrayColor = rgb 120 120 120

port tree : Signal Json.Value
port tree =
    Signal.map (\content -> UkkonenAlgorithm.buildTree content.string |> toJson) inputString.signal


type Button = Build | Back | Forward


inputString : Signal.Mailbox Content
inputString =
    Signal.mailbox noContent


inputButton : Signal.Mailbox Button
inputButton =
    Signal.mailbox Build


inputFieldStyle : Style
inputFieldStyle =
    let
        textDefaultStyle = Text.defaultStyle
    in
        { defaultStyle
            | padding = uniformly -6
            , outline = { color = lightGrayColor, width = uniformly 1, radius = 4 }
            , style = { textDefaultStyle | height = Just 25, color = lightGrayColor }
        }


inputField : Content -> Element
inputField =
    field inputFieldStyle (Signal.message inputString.address) "input string..."


visualizeButton : Element
visualizeButton = Graphics.Input.button (Signal.message inputButton.address Build) "build suffix tree"


main : Signal Html
main =
    Signal.map view inputString.signal


view : Content -> Html
view content =
    section
        [ id "visualization" ]
        [ h1 [] [ text "Visualization of Ukkonen's Algorithm" ],
          div [id "input-string"] [
            inputField content |> width 400 |> fromElement,
            span [id "input-button-wrapper"] [visualizeButton |> width 150 |> fromElement]
          ]
        ]
