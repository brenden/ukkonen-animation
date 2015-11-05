import Html exposing (pre, text)
import Html.Attributes exposing (class)
import Graphics.Element exposing (show)

import Ukkonen exposing (..)

main = let
    string = "abcabxabxyz"
    tree = Ukkonen.buildTree string
  in
    pre [] [text (Ukkonen.toString tree)]
