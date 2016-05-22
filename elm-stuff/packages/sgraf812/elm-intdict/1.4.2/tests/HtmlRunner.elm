module Main where

import Graphics.Element exposing (Element, leftAligned)
import ElmTest exposing (stringRunner)
import Text exposing (fromString)
import Test

main : Element
main =
  leftAligned (fromString (stringRunner Test.tests))
