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
import String exposing (..)
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
        (\( currentStep, steps, string ) ->
            case Array.get currentStep steps of
                Just state ->
                    let
                        _ = Debug.log (UkkonenTree.toString state.tree) state
                    in
                        toJson
                            state.tree
                            (String.slice
                                0
                                (currentStep + 1)
                                (String.fromList (Array.toList state.string))
                            )

                Nothing ->
                    toJson emptyTree string
        )
        (Signal.dropRepeats (Signal.map (\m -> ( m.currentStep, m.steps, m.string )) model))


type alias Model =
    { string : String
    , steps : Array UkkonenState
    , currentStep : Int
    , inputField : Content
    }


type Action
    = NoOp
    | InputFieldUpdate Content
    | Build String
    | Back
    | Forward


initialModel =
    { string = ""
    , steps = Array.empty
    , currentStep = 0
    , inputField = noContent
    }


inputString : Signal.Mailbox Content
inputString =
    Signal.mailbox noContent


inputButton : Signal.Mailbox Action
inputButton =
    Signal.mailbox NoOp


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


inputFieldUpdates : Signal Action
inputFieldUpdates =
    Signal.map (\content -> InputFieldUpdate content) inputString.signal


visualizeButton : Element
visualizeButton =
    Graphics.Input.button (Signal.message inputButton.address NoOp) "build suffix tree"


stringUpdates : Signal Action
stringUpdates =
    Signal.map2
        (\_ inputContent -> Build inputContent.string)
        inputButton.signal
        (Signal.sampleOn inputButton.signal inputString.signal)


leftButton : Element
leftButton =
    Graphics.Input.button (Signal.message currentStepUpdates.address Back) "◀"


rightButton : Element
rightButton =
    Graphics.Input.button (Signal.message currentStepUpdates.address Forward) "▶"


currentStepUpdates : Signal.Mailbox Action
currentStepUpdates =
    Signal.mailbox NoOp


main : Signal Html
main =
    Signal.map view model


actions : Signal Action
actions =
    Signal.mergeMany [ stringUpdates, currentStepUpdates.signal, inputFieldUpdates ]


model : Signal Model
model =
    Signal.foldp update initialModel actions


update : Action -> Model -> Model
update action model =
    case action of
        InputFieldUpdate content ->
            { model | inputField = content }

        Build string ->
            { model | string = string, steps = Array.fromList <| UkkonenAlgorithm.steps string }

        Back ->
            { model | currentStep = max (model.currentStep - 1) 0 }

        Forward ->
            { model | currentStep = min (model.currentStep + 1) ((Array.length model.steps) - 1) }

        NoOp ->
            model


view : Model -> Html
view model =
    section
        [ id "visualization" ]
        [ h1 [] [ text "Visualization of Ukkonen's Algorithm" ]
        , div
            [ id "input-string" ]
            [ inputField model.inputField |> width 400 |> fromElement
            , span [ id "input-button-wrapper" ] [ visualizeButton |> width 150 |> fromElement ]
            ]
        , div
            [ id "steps-wrapper" ]
            [ div
                [ id "side-box" ]
                [ div
                    [ id "narrative" ]
                    [ h2 [] [ text <| "Step " ++ Basics.toString model.currentStep ]
                    , p [] [ text "Some explanation blah blah blah" ]
                    ]
                , div
                    [ id "navigation" ]
                    [ span [ id "left-button-wrapper" ] [ leftButton |> width 50 |> fromElement ]
                    , span [ id "right-button-wrapper" ] [ rightButton |> width 50 |> fromElement ]
                    ]
                ]
            ]
        ]
