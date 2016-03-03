module UkkonenVisualization (..) where

import Html exposing (pre, text)
import Html.Attributes exposing (class)
import Graphics.Element exposing (show)
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

main =
    let
        string = "abcabxabcd"

        tree = UkkonenAlgorithm.buildTree string
    in
        pre [] [ text string ]
