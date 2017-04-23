module Tests (..) where

import Debug
import Array
import Html exposing (..)
import List exposing (..)
import String
import UkkonenTree exposing (..)
import UkkonenAlgorithm exposing (..)


main =
    let
        testWords =
            [ "banana"
            , "tctcatcaa#ggaaccattg@tccatctcgc"
            , "abacabadabacabae"
            , "asdfsfasdfasdfx"
            , "aaaaaaa"
            , "abababa"
            , "abcabxabcd"
            , "abcabcdefbcabcd"
            , "abcabcdefhabcabcdw"
            , "abcabcdefbcabcd"
            ]
    in
        text <|
            if (all checkValidSuffixTree testWords) then
                "All tests passed."
            else
                "Some tests failed. Check console for details."


checkValidSuffixTree : String -> Bool
checkValidSuffixTree input =
    let
        finalState =
            List.head <| List.reverse <| UkkonenAlgorithm.steps (input ++ "$")
    in
        case finalState of
            Just finalState ->
                let
                    suffixes =
                        Debug.log "expected" <| List.sort <| suffixesOfString input

                    suffixesFromTree =
                        Debug.log "  actual" <|
                            List.sort <|
                                UkkonenTree.suffixes
                                    finalState.tree
                                    0
                                    (String.fromList <| Array.toList <| finalState.string)
                in
                    Debug.log "matches" <| suffixes == List.sort suffixesFromTree

            Nothing ->
                False


suffixesOfString : String -> List String
suffixesOfString string =
    let
        len =
            String.length string
    in
        List.map
            (\n -> String.right n string)
            [0..len]
