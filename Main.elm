import Html exposing (pre, text)
import Html.Attributes exposing (class)
import Graphics.Element exposing (show)

import Ukkonen exposing (..)

main = let
    string = "abcabxabcd"
    tree = Ukkonen.buildTree string
  in
    pre [] [text (Ukkonen.toString tree)]
