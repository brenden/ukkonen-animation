module Test (tests) where


import Test.IntDict

import ElmTest exposing (..)


tests : Test
tests =
  suite "elm-intdict test suite"
    [ Test.IntDict.tests
    ]
