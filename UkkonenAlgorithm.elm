module UkkonenAlgorithm (buildTree) where

import UkkonenTree exposing (..)
import Array exposing (..)
import String exposing (..)
import Debug

type alias ActivePoint = {
  nodeId:NodeId,
  edge:Maybe (Char, Int) }

type alias UkkonenState = {
  tree:UkkonenTree,
  remainder:Int,
    activePoint:ActivePoint,
  string:Array Char,
  lastSplitNode:Maybe NodeId }


-- Returns the initial state for the Ukkonen algorithm
initialState : UkkonenState
initialState = {
  tree = emptyTree,
  remainder = 1,
  activePoint = {
    nodeId = 0,
    edge = Nothing
  },
  string = Array.empty,
  lastSplitNode = Nothing }


-- Add another character to the tree
insert : UkkonenState -> Char -> UkkonenState
insert initState newChar = let
    state = { initState | string <- push newChar initState.string }
  in
    insert' state newChar

insert' : UkkonenState -> Char -> UkkonenState
insert' state newChar = let
    -- Get convenient references to the state record's fields
    {tree, remainder, activePoint, string, lastSplitNode} = state

    -- Get the index of the character being inserted
    i = Array.length string - 1
  in
    case activePoint.edge of

      -- The case that there's currently no active edge, i.e the active point
      -- is a node in the suffix tree
      Nothing ->
        case getEdge tree activePoint.nodeId newChar of

          -- If an edge starting with the new character already exists at this
          -- node, then set the active edge to that edge.
          Just edge -> { state |
            activePoint <- walkEdge tree string activePoint newChar 1,
            remainder <- state.remainder + 1 }

          -- Otherwise we need to create a new edge pointing from this node
          Nothing -> let
              (newTree, newId) = addNode tree
              newTree2 = setEdge newTree
                                 activePoint.nodeId
                                 newId
                                 newChar
                                 i
                                 EndOfString
            in
              { state |
                tree <- newTree2 }
                --activePoint <- walkEdge newTree2 string activePoint newChar 1 }

      -- The case that there is an active edge defined
      Just (edgeChar, edgeSteps) ->
        case getEdge tree activePoint.nodeId edgeChar of
          Just activeEdge ->
            let
              -- This is the index of the input string that the current edge
              -- location points to.
              currentStringIndex = activeEdge.labelStart + edgeSteps

              -- The character that the current edge location represents
              c = getChar string currentStringIndex
            in
              -- If the new suffix is already implicitly present in the
              -- tree, then step forward on the active edge and increment
              -- the remainder.
              if newChar == c then
                { state |
                  activePoint <- walkEdge tree
                                           string
                                           activePoint
                                           edgeChar
                                           (edgeSteps + 1),
                  remainder <- state.remainder + 1 }

              -- Otherwise, the new character being inserted is different from the
              -- character pointed to by the active point, so the active edge needs
              -- to be split.
              else let
                  (newTree1, newNodeId1) = addNode tree
                  (newTree2, newNodeId2) = addNode newTree1
                  newTree3 = setEdge newTree2
                                     activeEdge.pointingTo
                                     newNodeId1
                                     newChar
                                     i
                                     EndOfString
                  newTree4 = setEdge newTree3
                                     activeEdge.pointingTo
                                     newNodeId2
                                     c
                                     currentStringIndex
                                     EndOfString

                  -- Common edge shared by the suffixes
                  newTree5 = setEdge newTree4
                                     activePoint.nodeId
                                     activeEdge.pointingTo
                                     edgeChar
                                     activeEdge.labelStart
                                     (Definite currentStringIndex)

                  newTree6 = case lastSplitNode of
                    Just nodeId -> setSuffixLink newTree5
                                                 nodeId
                                                 activeEdge.pointingTo
                    Nothing -> newTree5

                  -- Update the active point
                  newActivePoint = if activePoint.nodeId == 0 then
                      { activePoint
                        | edge <- Just ((getChar string
                                                 (i - state.remainder + 2)),
                                        edgeSteps - 1) }
                    else let
                        activeNode = getNode tree activePoint.nodeId
                      in
                        case activeNode.suffixLink of
                          Just nodeId -> { activePoint | nodeId <- nodeId }
                          Nothing -> { activePoint | nodeId <- 0 }

                  -- Update the state
                  newState = { state |
                    tree <- newTree6,
                    activePoint <- newActivePoint,
                    remainder <- state.remainder - 1,
                    lastSplitNode <- Just activeEdge.pointingTo }
                in
                  -- Recurse to insert the next remaining suffix
                    insert' newState newChar

          -- Insert the new activeEdge if it doesn't exist yet
          Nothing -> let
              (treeWithNextSuffixNode, nextSuffixNode) = addNode tree
              newActivePoint = { activePoint | edge <- Nothing }
            in
              { state | tree <- setEdge treeWithNextSuffixNode
                                        activePoint.nodeId
                                        nextSuffixNode
                                        newChar
                                        i
                                        EndOfString,
                        activePoint <- newActivePoint }

-- Move the active point n steps onto the edge that's labeled with char c. If
-- that edge ends, continue onto x, the node it points to, and then onto the
-- edge starting at x which is labeled with char c. Continue this process until
-- all n steps have been taken.
walkEdge : UkkonenTree ->
            Array Char ->
            ActivePoint ->
            Char ->
            Int ->
            ActivePoint
walkEdge tree string activePoint char n = case getEdge tree
                                                       activePoint.nodeId
                                                       char of
    Just activeEdge ->
      let
        activeEdgeLength = case activeEdge.labelEnd of
          EndOfString -> (Array.length string) - activeEdge.labelStart
          Definite end -> end - activeEdge.labelStart
      in
        if n <= activeEdgeLength then
          { activePoint | edge <- Just (char, n) }
        else
          walkEdge tree
                    string
                    { activePoint | nodeId <- activeEdge.pointingTo }
                    (getChar string (activeEdge.labelStart + activeEdgeLength))
                    (n - activeEdgeLength)

    Nothing -> Debug.crash ("Tried to reference edge "
      ++ (Basics.toString (activePoint.nodeId, char))
      ++ ", which doesn't exist")


-- Runs the given string through the Ukkonen algorithm and retuns the
-- final state
buildTree : String -> UkkonenTree
buildTree string = buildTree' initialState (String.toList string)

buildTree' currentState string =
  case string of
    [] -> currentState.tree
    c::rest -> buildTree' (insert currentState c) rest


-- Convenience method for looking up the character at the given position in the
-- input string
getChar : Array Char -> Int -> Char
getChar str i = case Array.get i str of
  Just c -> c
  Nothing -> Debug.crash <| "Tried to look up index " ++ (Basics.toString i) ++
    ", which is outside the bounds of " ++ (Basics.toString str)
