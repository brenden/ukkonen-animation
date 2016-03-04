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
port tree = let
    string = "abcabxabcd"

    sufTree = UkkonenAlgorithm.buildTree string
  in
    Signal.map (\( w, h ) -> toJson sufTree) Window.dimensions

inputString : Signal.Mailbox Content
inputString = Signal.mailbox noContent

main : Signal Element
main =
    Signal.map (field defaultStyle (Signal.message inputString.address) "Input string") inputString.signal
