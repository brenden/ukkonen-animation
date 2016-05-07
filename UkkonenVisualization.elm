module UkkonenVisualization (..) where

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id, class, disabled)
import Html.Events exposing (onClick)
import Text
import Color exposing (..)
import Dict
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
                    treeJson
                        state.tree
                        state.activePoint
                        (String.slice
                            0
                            (currentStep + 1)
                            (String.fromList (Array.toList state.string))
                        )

                Nothing ->
                    Json.null
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


leftButton : Bool -> Html
leftButton enabled =
    Html.button [ onClick currentStepUpdates.address Back, disabled <| not enabled ] [ text "◀" ]


rightButton : Bool -> Html
rightButton enabled =
    Html.button [ onClick currentStepUpdates.address Forward, disabled <| not enabled ] [ text "▶" ]


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
            if string == "" then
                model
            else
                let
                    steps = Array.fromList (initialState :: UkkonenAlgorithm.steps string)
                in
                    { model | string = string, steps = steps, currentStep = 0 }

        Back ->
            { model | currentStep = max (model.currentStep - 1) 0 }

        Forward ->
            { model | currentStep = min (model.currentStep + 1) ((Array.length model.steps) - 1) }

        NoOp ->
            model


view : Model -> Html
view model =
    let
        leftButtonEnabled = model.currentStep > 0

        rightButtonEnabled = model.currentStep < (Array.length model.steps) - 1
    in
        section
            [ id "visualization" ]
            [ h1 [] [ text "A Visualization of Ukkonen's Algorithm" ]
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
                        , p
                            []
                            [ text
                                <| case Array.get model.currentStep model.steps of
                                    Just state ->
                                        (Basics.toString state.activePoint) ++ "\n \n" ++ (Basics.toString state.remainder)

                                    Nothing ->
                                        ""
                            ]
                        ]
                    , div
                        [ id "navigation" ]
                        [ span [ id "left-button-wrapper" ] [ leftButton leftButtonEnabled ]
                        , span [ id "right-button-wrapper" ] [ rightButton rightButtonEnabled ]
                        ]
                    ]
                ]
            ]


{-| Prints out a JSON representation of the tree
-}
treeJson : UkkonenTree -> ActivePoint -> String -> Json.Value
treeJson tree activePoint string =
    treeJson' 0 tree activePoint string


treeJson' : Int -> UkkonenTree -> ActivePoint -> String -> Json.Value
treeJson' rootId tree activePoint string =
    let
        root = getNode rootId tree

        isActivePoint = (activePoint.nodeId == rootId)
    in
        Json.object
            [ ( "id", Json.int rootId )
            , ( "suffixLink"
              , case root.suffixLink of
                    Just n ->
                        Json.int n

                    Nothing ->
                        Json.null
              )
            , ( "isActivePoint", Json.bool isActivePoint )
            , ( "children"
              , Json.object
                    (List.map
                        (\( c, edge ) ->
                            let
                                labelEnd =
                                    case edge.labelEnd of
                                        Definite l ->
                                            l

                                        EndOfString ->
                                            String.length string
                            in
                                ( fromChar c
                                , Json.object
                                    [ ( "label", Json.string <| String.slice edge.labelStart labelEnd string )
                                    , ( "pointingTo", treeJson' edge.pointingTo tree activePoint string )
                                    , ( "edgeSteps"
                                      , case activePoint.edge of
                                            Just ( ac, edgeSteps ) ->
                                                if (isActivePoint && c == ac) then
                                                    Json.int edgeSteps
                                                else
                                                    Json.null

                                            Nothing ->
                                                Json.null
                                      )
                                    ]
                                )
                        )
                        (Dict.toList root.edges)
                    )
              )
            ]
