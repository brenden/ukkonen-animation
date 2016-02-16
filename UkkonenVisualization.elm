module UkkonenVisualization (..) where

import Html exposing (pre, text)
import Html.Attributes exposing (class)
import Graphics.Element exposing (show)
import Json.Encode as Json
import Window
import UkkonenTree exposing (..)
import UkkonenAlgorithm exposing (..)


port tree : Signal Json.Value
port tree =
    Signal.map (\( w, h ) -> Json.list [ Json.int w, Json.int h ]) Window.dimensions


main =
    let
        string = "abcabxabcd"

        tree = UkkonenAlgorithm.buildTree string
    in
        pre [] [ text (UkkonenTree.toString tree) ]
