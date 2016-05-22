module Main where

import Task exposing (..)

import ElmTest exposing (..)
import Console

import Test

port runner : Signal (Task x ())
port runner =
  Console.run (consoleRunner Test.tests)
