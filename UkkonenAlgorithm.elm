module UkkonenAlgorithm (steps, UkkonenState, initialState, ActivePoint) where

import UkkonenTree exposing (..)
import Array exposing (..)
import List exposing (..)
import String exposing (..)
import Debug


type alias ActivePoint =
    { nodeId : NodeId
    , edge : Maybe ( Char, Int )
    }


type alias UkkonenState =
    { tree : UkkonenTree
    , remainder : Int
    , activePoint : ActivePoint
    , string : Array Char
    , lastSplitNode : Maybe NodeId
    , charsAdded : Int
    }


{-| Returns the initial state for the Ukkonen algorithm
-}
initialState : UkkonenState
initialState =
    { tree = emptyTree
    , remainder = 1
    , activePoint =
        { nodeId = 0
        , edge = Nothing
        }
    , string = Array.empty
    , lastSplitNode = Nothing
    , charsAdded = 0
    }


{-| Add another character to the tree
-}
insert : Char -> UkkonenState -> List UkkonenState
insert newChar initState =
    let
        state = { initState | string = push newChar initState.string }
    in
        insert' newChar state


insert' : Char -> UkkonenState -> List UkkonenState
insert' newChar state =
    let
        -- Get convenient references to the state record's fields
        { tree, remainder, activePoint, string, lastSplitNode, charsAdded } = normalizeActivePoint state

        -- Get the index of the character being inserted
        i = Array.length string - 1

    in
        case activePoint.edge of
            -- The case that there's currently no active edge, i.e the active point
            -- is a node in the suffix tree
            Nothing ->
                case getEdge activePoint.nodeId newChar tree of
                    -- If an edge starting with the new character already exists at this
                    -- node, then set the active edge to that edge.
                    Just edge ->
                        let
                            newActivePoint =
                                { activePoint
                                    | edge = Just ( newChar, 1 )
                                }
                        in
                            [ normalizeActivePoint
                                { state
                                    | activePoint = newActivePoint
                                    , remainder = state.remainder + 1
                                    , lastSplitNode = Nothing
                                    , charsAdded = charsAdded + 1
                                }
                            ]

                    -- Otherwise we need to create a new edge pointing from this node
                    Nothing ->
                        let
                            ( newTree, newId ) = addNode tree

                            newTree2 =
                                setEdge
                                    activePoint.nodeId
                                    newId
                                    newChar
                                    i
                                    EndOfString
                                    newTree
                        in
                            if state.remainder == 1 then
                                let
                                    newState =
                                        { state
                                            | tree = newTree2
                                            , lastSplitNode = Nothing
                                            , charsAdded = charsAdded + 1
                                        }
                                in
                                    [ newState ]
                            else
                                let
                                    newState =
                                        normalizeActivePoint <|
                                            { state
                                                | tree = newTree2
                                                , activePoint = nextActivePoint state tree
                                                , remainder = state.remainder - 1
                                            }
                                in
                                    newState :: (insert' newChar newState)

            -- The case that there is an active edge defined
            Just ( edgeChar, edgeSteps ) ->
                case getEdge activePoint.nodeId edgeChar tree of
                    Just activeEdge ->
                        let
                            -- This is the index of the input string that the current edge
                            -- location points to.
                            currentStringIndex = activeEdge.labelStart + edgeSteps

                            -- The character that the current active point represents
                            activePointChar = getChar currentStringIndex string
                        in
                            -- If the new suffix is already implicitly present in the
                            -- tree, then step forward on the active edge and increment
                            -- the remainder.
                            if newChar == activePointChar then
                                [ normalizeActivePoint
                                    { state
                                        | activePoint = { activePoint | edge = Just ( edgeChar, edgeSteps + 1 ) }
                                        , remainder = state.remainder + 1
                                        , charsAdded = charsAdded + 1
                                        , lastSplitNode = Nothing
                                    }
                                ]
                            else
                                -- Split the active edge
                                let
                                    -- Add one of the new nodes
                                    ( newTree1, newNodeId1 ) = addNode tree

                                    -- Add the other one
                                    ( newTree2, newNodeId2 ) = addNode newTree1

                                    -- Edge formed by the newly inserted character
                                    newTree3 =
                                        setEdge
                                            newNodeId1
                                            newNodeId2
                                            newChar
                                            i
                                            EndOfString
                                            newTree2

                                    -- Edge formed by the original characters that follow the active point
                                    newTree4 =
                                        setEdge
                                            newNodeId1
                                            activeEdge.pointingTo
                                            activePointChar
                                            currentStringIndex
                                            activeEdge.labelEnd
                                            newTree3

                                    -- Common edge shared by the suffixes
                                    newTree5 =
                                        setEdge
                                            activePoint.nodeId
                                            newNodeId1
                                            edgeChar
                                            activeEdge.labelStart
                                            (Definite currentStringIndex)
                                            newTree4

                                    -- Add a suffix link if we've already split a node during the current insertion
                                    newTree6 =
                                        case lastSplitNode of
                                            Just nodeId ->
                                                setSuffixLink
                                                    nodeId
                                                    newNodeId1
                                                    newTree5

                                            Nothing ->
                                                newTree5

                                    -- Update the active point
                                    newActivePoint =
                                        if activePoint.nodeId == 0 then
                                            { activePoint
                                                | edge =
                                                    Just
                                                        ( getChar
                                                            (i - state.remainder + 2)
                                                            string
                                                        , edgeSteps - 1
                                                        )
                                            }
                                        else
                                            nextActivePoint state tree

                                    -- Update the state
                                    newState =
                                        normalizeActivePoint
                                            { state
                                                | tree = newTree6
                                                , activePoint = newActivePoint
                                                , remainder = state.remainder - 1
                                                , lastSplitNode = Just newNodeId1
                                            }
                                in
                                    -- Recurse to insert the next remaining suffix
                                    newState :: (insert' newChar newState)

                    Nothing ->
                        Debug.crash "active_edge is set to a nonexistent edge."


{-| Update the active point to reflect the next suffix to be inserted. If a
    suffix link exists at the current node, just follow that. Otherwise go
    back to the root node and set active length to be the length of the next
    suffix.
-}
nextActivePoint : UkkonenState -> UkkonenTree -> ActivePoint
nextActivePoint state tree =
    let
        activePoint = state.activePoint

        activeNode = getNode activePoint.nodeId tree
    in
        case activeNode.suffixLink of
            Just nodeId ->
                { activePoint | nodeId = nodeId }

            Nothing ->
                case activePoint.edge of
                    Nothing ->
                         { activePoint
                            | nodeId = 0
                            , edge =
                              if state.remainder == 1 then
                                  Nothing
                              else
                                  Just
                                    ( getChar
                                        (Array.length state.string - state.remainder + 1)
                                        state.string
                                    , state.remainder - 2
                                    )
                        }

                    Just ( edgeChar, edgeSteps ) ->
                        { activePoint
                            | nodeId = 0
                            , edge =
                                Just
                                    ( getChar
                                        (Array.length state.string - state.remainder + 1)
                                        state.string
                                    , state.remainder - 2
                                    )
                        }


{-| In some cases the active length could be greater than the length of the
    active edge. In this situation, we normalize the active point by moving
    the active node to the node pointed to by the active edge, and then
    decrementing the active length by the length of the active edge. We repeat
    this process until the active length is lower than the length of the
    active edge.
-}
normalizeActivePoint : UkkonenState -> UkkonenState
normalizeActivePoint state =
    let
        activePoint = state.activePoint
    in
        case activePoint.edge of
            Nothing ->
                state

            Just ( edgeChar, edgeSteps ) ->
                if edgeSteps == 0 then
                    { state
                        | activePoint = { activePoint | edge = Nothing }
                    }
                else
                    case
                        getEdge
                            state.activePoint.nodeId
                            edgeChar
                            state.tree
                    of
                        Just activeEdge ->
                            let
                                stringLen = Array.length state.string

                                activeEdgeLength =
                                    case activeEdge.labelEnd of
                                        EndOfString ->
                                            stringLen - activeEdge.labelStart

                                        Definite end ->
                                            end - activeEdge.labelStart
                            in
                                if edgeSteps < activeEdgeLength then
                                    state
                                else
                                    normalizeActivePoint
                                        { state
                                            | activePoint =
                                                { activePoint
                                                    | nodeId = activeEdge.pointingTo
                                                    , edge = Just ( getChar (stringLen - state.remainder + 1) state.string, edgeSteps - activeEdgeLength )
                                                }
                                        }

                        Nothing ->
                            state


{-| Creates a list of all intermediary states encountered while building the
    suffix tree
-}
steps : String -> List UkkonenState
steps string =
    List.reverse
        <| List.foldl
            (\c stepList ->
                case head stepList of
                    Just lastStep ->
                        List.append (List.reverse (insert c lastStep)) stepList

                    Nothing ->
                        insert c initialState
            )
            []
            (String.toList string)


{-| Convenience method for looking up the character at the given position in
    the input string
-}
getChar : Int -> Array Char -> Char
getChar i str =
    case Array.get i str of
        Just c ->
            c

        Nothing ->
            Debug.crash
                <| "Tried to look up index "
                ++ (Basics.toString i)
                ++ ", which is outside the bounds of "
                ++ (Basics.toString str)
