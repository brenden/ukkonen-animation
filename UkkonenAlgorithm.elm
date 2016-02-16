module UkkonenAlgorithm (buildTree) where

import UkkonenTree exposing (..)
import Array exposing (..)
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
    }


{-| Add another character to the tree
-}
insert : Char -> UkkonenState -> UkkonenState
insert newChar initState =
    let
        state = { initState | string = push newChar initState.string }
    in
        insert' newChar state


insert' : Char -> UkkonenState -> UkkonenState
insert' newChar state =
    let
        -- Get convenient references to the state record's fields
        { tree, remainder, activePoint, string, lastSplitNode } = state

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
                        { state
                            | activePoint = walkEdge string activePoint newChar 1 tree
                            , remainder = state.remainder + 1
                        }

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
                            { state
                                | tree = newTree2
                            }

            -- The case that there is an active edge defined
            Just ( edgeChar, edgeSteps ) ->
                case getEdge activePoint.nodeId edgeChar tree of
                    Just activeEdge ->
                        let
                            -- This is the index of the input string that the current edge
                            -- location points to.
                            currentStringIndex = activeEdge.labelStart + edgeSteps

                            -- The character that the current edge location represents
                            c = getChar currentStringIndex string
                        in
                            -- If the new suffix is already implicitly present in the
                            -- tree, then step forward on the active edge and increment
                            -- the remainder.
                            if newChar == c then
                                { state
                                    | activePoint =
                                        walkEdge
                                            string
                                            activePoint
                                            edgeChar
                                            (edgeSteps + 1)
                                            tree
                                    , remainder = state.remainder + 1
                                }
                            else
                                -- Split the active edge
                                let
                                    ( newTree1, newNodeId1 ) = addNode tree

                                    ( newTree2, newNodeId2 ) = addNode newTree1

                                    newTree3 =
                                        setEdge
                                            activeEdge.pointingTo
                                            newNodeId1
                                            newChar
                                            i
                                            EndOfString
                                            newTree2

                                    newTree4 =
                                        setEdge
                                            activeEdge.pointingTo
                                            newNodeId2
                                            c
                                            currentStringIndex
                                            EndOfString
                                            newTree3

                                    -- Common edge shared by the suffixes
                                    newTree5 =
                                        setEdge
                                            activePoint.nodeId
                                            activeEdge.pointingTo
                                            edgeChar
                                            activeEdge.labelStart
                                            (Definite currentStringIndex)
                                            newTree4

                                    newTree6 =
                                        case lastSplitNode of
                                            Just nodeId ->
                                                setSuffixLink
                                                    nodeId
                                                    activeEdge.pointingTo
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
                                            let
                                                activeNode = getNode activePoint.nodeId tree
                                            in
                                                case activeNode.suffixLink of
                                                    Just nodeId ->
                                                        { activePoint | nodeId = nodeId }

                                                    Nothing ->
                                                        { activePoint | nodeId = 0 }

                                    -- Update the state
                                    newState =
                                        { state
                                            | tree = newTree6
                                            , activePoint = newActivePoint
                                            , remainder = state.remainder - 1
                                            , lastSplitNode = Just activeEdge.pointingTo
                                        }
                                in
                                    -- Recurse to insert the next remaining suffix
                                    insert' newChar newState

                    -- Insert the new activeEdge if it doesn't exist yet
                    Nothing ->
                        let
                            ( treeWithNextSuffixNode, nextSuffixNode ) = addNode tree

                            newActivePoint = { activePoint | edge = Nothing }
                        in
                            { state
                                | tree =
                                    setEdge
                                        activePoint.nodeId
                                        nextSuffixNode
                                        newChar
                                        i
                                        EndOfString
                                        treeWithNextSuffixNode
                                , activePoint = newActivePoint
                            }


{-| Move the active point n steps onto the edge that's labeled with char c. If
    that edge ends, continue onto x, the node it points to, and then onto the
    edge starting at x which is labeled with char c. Continue this process
    until all n steps have been taken.
-}
walkEdge : Array Char -> ActivePoint -> Char -> Int -> UkkonenTree -> ActivePoint
walkEdge string activePoint char n tree =
    case
        getEdge
            activePoint.nodeId
            char
            tree
    of
        Just activeEdge ->
            let
                activeEdgeLength =
                    case activeEdge.labelEnd of
                        EndOfString ->
                            (Array.length string) - activeEdge.labelStart

                        Definite end ->
                            end - activeEdge.labelStart
            in
                if n <= activeEdgeLength then
                    { activePoint | edge = Just ( char, n ) }
                else
                    walkEdge
                        string
                        { activePoint | nodeId = activeEdge.pointingTo }
                        (getChar (activeEdge.labelStart + activeEdgeLength) string)
                        (n - activeEdgeLength)
                        tree

        Nothing ->
            Debug.crash
                ("Tried to reference edge "
                    ++ (Basics.toString ( activePoint.nodeId, char ))
                    ++ ", which doesn't exist"
                )


{-| Runs the given string through the Ukkonen algorithm and retuns the
    final state
-}
buildTree : String -> UkkonenTree
buildTree string =
    buildTree' initialState (String.toList string)


buildTree' currentState string =
    case string of
        [] ->
            currentState.tree

        c :: rest ->
            buildTree' (insert c currentState) rest


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
