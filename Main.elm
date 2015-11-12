import Html exposing (pre, text)
import Html.Attributes exposing (class)
import Graphics.Element exposing (show)

import UkkonenTree exposing (..)
import UkkonenAlgorithm exposing (..)

main = let
    string = "abcabxabcd"
    tree = UkkonenAlgorithm.buildTree string
  in
    pre [] [text (UkkonenTree.toString tree)]
