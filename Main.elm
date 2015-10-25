import Html exposing (span, text)
import Html.Attributes exposing (class)
import Graphics.Element exposing (show)

import Ukkonen exposing (..)

main = let
    state1 = Ukkonen.initialState
    state2 = Ukkonen.insert state1 'a'
    state3 = Ukkonen.insert state2 'b'
  in
    show state3.remainder
