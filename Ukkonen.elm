import IntDict exposing (..)
import Dict exposing (..)
import Array exposing (..)
import String exposing (..)
import Debug

type alias NodeId = Int
type alias UkkonenTree = IntDict UkkonenNode

type alias UkkonenNode = {
  edges:Dict Char UkkonenEdge,
  suffixLink:Maybe NodeId}

type alias UkkonenEdge = {
  pointingTo:NodeId,
  labelStart:Int,
  labelEnd:ClosingIndex }

type alias ActivePoint = {
  nodeId:NodeId,
  edge:Maybe (Char, Int) }

type alias UkkonenState = {
  tree: UkkonenTree,
  remainder:Int,
  activePoint:ActivePoint,
  string:Array Char }

type ClosingIndex = Definite Int | EndOfString

-- Add another character to the tree
insert : UkkonenState -> Char -> UkkonenState
insert state char =
  let

    -- Append the new character to the tree's input string
    state = { state | string <- push char state.string }

    -- Get convenient references to the state record's fields
    {tree, remainder, activePoint, string} = state

    -- Get the index of the character being inserted
    i = Array.length string - 1
  in
    case activePoint.edge of

      -- The case that there's currently no active edge, i.e the active point
      -- is a node in the suffix tree
      Nothing ->
        case getEdge tree activePoint.nodeId char of

          -- If an edge starting with the new character already exists at this
          -- node, then set the active edge to that edge.
          Just edge -> { state |
            activePoint <- apSetEdge tree activePoint char 1,
            remainder <- tree.remainder + 1 }

          -- Otherwise we need to create a new edge pointing from this node
          Nothing -> let
              (newTree, newId) = addNode tree
            in
              { state |
                tree <- addEdge newTree
                                activePoint.nodeId
                                newId
                                char
                                i
                                EndOfString,
                activePoint <- apSetEdge tree activePoint char 1 }

      -- The case that there is an active edge
      Just (edgeChar, edgeSteps) ->
        case getEdge tree activePoint.nodeId edgeChar of
          Just edge ->
            let

              -- This is the index of the input string that the current edge
              -- location points to.
              currentStringIndex = edge.labelStart + edgeSteps
            in
              case Array.get currentStringIndex tree.string of
                Just c ->

                  -- If the new suffix is already implicitly present in the
                  -- tree, then step forward on the active edge and increment
                  -- the remainder.
                  if char == c then
                    { state |
                      activePoint <- apSetEdge tree
                                               activePoint
                                               edgeChar
                                               (edgeSteps + 1),
                      remainder <- tree.remainder + 1 }

                  -- Otherwise, the active edge needs to be split, forming two
                  -- new edges.
                  else let
                      (newTree1, newNodeId1) = addNode tree
                      (newTree2, newNodeId2) = addNode newTree1
                      newTree3 = addEdge newTree2
                                         edge.pointingTo
                                         newNodeId1
                                         char
                                         i
                                         EndOfString
                      newTree4 = addEdge newTree3
                                         edge.pointingTo
                                         newNodeId2
                                         c
                                         currentStringIndex
                                         EndOfString

                      -- Common edge shared by the suffixes
                      newTree5 = addEdge newTree4
                                         activePoint.nodeId
                                         edge.pointingTo
                                         edgeChar
                                         edge.labelStart
                                         (Definite currentStringIndex)
                    in
                      { state |
                        tree <- newTree5,
                        remainder <- tree.remainder - 1 }
                Nothing ->
                  Debug.crash "Edge index isn't within the input string"
          Nothing ->
            Debug.crash "Active point is set to an edge that doesn't exist"


-- Get the edge that starts with `char`
getEdge : UkkonenTree -> NodeId -> Char -> Maybe UkkonenEdge
getEdge tree nodeId char = case IntDict.get nodeId tree of
  Just node -> Dict.get char node.edges
  Nothing -> Debug.crash "Active point is set to a node that doesn't exist"


-- Add `edge` that starts with `char`
addEdge : UkkonenTree -> NodeId -> NodeId -> Char -> Int -> ClosingIndex
          -> UkkonenTree
addEdge tree fromId toId char labelStart labelEnd = let
    node = getNode tree fromId
    newEdges = Dict.insert char
                           (createEdge toId labelStart labelEnd)
                           node.edges
    newNode = { node | edges <- newEdges }
  in
    IntDict.insert fromId newNode tree


-- Get the node associated with given id
getNode : UkkonenTree -> NodeId -> UkkonenNode
getNode tree nodeId = case IntDict.get nodeId tree of
  Just node -> node
  Nothing -> Debug.crash "Tried to reference a node that does't exist"


-- Add a new node to the graph
addNode : UkkonenTree -> (UkkonenTree, NodeId)
addNode tree = let
    count = IntDict.size tree
  in
    (IntDict.insert count createNode tree, count)


-- Creates a new empty node
createNode : UkkonenNode
createNode = {edges = Dict.empty, suffixLink = Nothing}


-- Creates a new unbound edge
createEdge : NodeId -> Int -> ClosingIndex -> UkkonenEdge
createEdge nodeId labelStart labelEnd = {
  pointingTo = nodeId,
  labelStart = labelStart,
  labelEnd = labelEnd }


-- Move `activePoint` onto the edge that starts with `char`
-- TODO handle walking off edge
apSetEdge : UkkonenTree -> ActivePoint-> Char -> Int -> ActivePoint
apSetEdge tree activePoint char labelStart =
  { activePoint | edge <- Just (char, labelStart) }
