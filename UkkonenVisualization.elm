module UkkonenVisualization (..) where

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id, class)
import Text
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Graphics.Input.Field exposing (..)
import Json.Encode as Json
import Window
import UkkonenTree exposing (..)
import UkkonenAlgorithm exposing (..)


baseColor =
    rgb 57 75 169


lightGrayColor =
    rgb 120 120 120


port tree : Signal Json.Value
port tree =
    Signal.map
        (\inputContent -> UkkonenAlgorithm.buildTree inputContent.string |> toJson)
        (Signal.sampleOn inputButton.signal inputString.signal)


type alias Model =
    { input : String
    , steps : Array UkkonenState
    , currentStep : Int
    }


type Action
    = Build String
    | Back
    | Forward


inputString : Signal.Mailbox Content
inputString =
    Signal.mailbox noContent


inputButton : Signal.Mailbox Bool
inputButton =
    Signal.mailbox False


inputFieldStyle : Style
inputFieldStyle =
    let
        textDefaultStyle = Text.defaultStyle
    in
        { defaultStyle
            | padding = uniformly -6
            , outline = { color = lightGrayColor, width = uniformly 1, radius = 4 }
            , style = { textDefaultStyle | height = Just 25, color = lightGrayColor }
        }


inputField : Content -> Element
inputField =
    field inputFieldStyle (Signal.message inputString.address) "input string..."


visualizeButton : Element
visualizeButton =
    Graphics.Input.button (Signal.message inputButton.address True) "build suffix tree"


actions : Signal Action
actions =
    Signal.map2
        (\_ inputContent -> Build inputContent.string)
        inputButton.signal
        (Signal.sampleOn inputButton.signal inputString.signal)


main : Signal Html
main =
    Signal.map view inputString.signal


update : Action -> Model -> Model
update action model =
    case action of
        Build input ->
            { model | input = input, steps = fromList <| UkkonenAlgorithm.steps input }

        Back ->
            { model | currentStep = model.currentStep - 1 }

        Forward ->
            { model | currentStep = model.currentStep + 1 }


view : Content -> Html
view content =
    section
        [ id "visualization" ]
        [ h1 [] [ text "Visualization of Ukkonen's Algorithm" ]
        , div
            [ id "input-string" ]
            [ inputField content |> width 400 |> fromElement
            , span [ id "input-button-wrapper" ] [ visualizeButton |> width 150 |> fromElement ]
            ]
        ]
