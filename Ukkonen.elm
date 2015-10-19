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
  string:Array Char,
  lastSplitNode:Maybe NodeId }

type ClosingIndex = Definite Int | EndOfString

-- Add another character to the tree
insert : UkkonenState -> Char -> UkkonenState
insert state newChar =
  let
    -- Append the new character to the tree's input string
    state = { state | string <- push newChar state.string }

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
            activePoint <- apSetEdge tree activePoint newChar 1,
            remainder <- tree.remainder + 1 }

          -- Otherwise we need to create a new edge pointing from this node
          Nothing -> let
              (newTree, newId) = addNode tree
            in
              { state |
                tree <- addEdge newTree
                                activePoint.nodeId
                                newId
                                newChar
                                i
                                EndOfString,
                activePoint <- apSetEdge tree activePoint newChar 1 }

      -- The case that there is an active edge
      Just (edgeChar, edgeSteps) ->
        let
          activeEdge = Maybe.withDefault
            (Debug.crash "Active point is set to an edge that doesn't exist")
            (getEdge tree activePoint.nodeId edgeChar)

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
              activePoint <- apSetEdge tree
                                       activePoint
                                       edgeChar
                                       (edgeSteps + 1),
              remainder <- tree.remainder + 1 }

          -- Otherwise, the new character being inserted is different from the
          -- character pointed to by the active point, so the active edge needs
          -- to be split.
          else let
              
              (newTree1, newNodeId1) = addNode tree
              (newTree2, newNodeId2) = addNode newTree1
              newTree3 = addEdge newTree2
                                 activeEdge.pointingTo
                                 newNodeId1
                                 newChar
                                 i
                                 EndOfString
              newTree4 = addEdge newTree3
                                 activeEdge.pointingTo
                                 newNodeId2
                                 c
                                 currentStringIndex
                                 EndOfString

              -- Common edge shared by the suffixes
              newTree5 = addEdge newTree4
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
                    | edge <- Just (getChar string (i - 1), edgeSteps - 1) }
                else let
                    activeNode = getNode tree activePoint.nodeId
                  in
                    case activeNode.suffixLink of
                      Just nodeId -> { activePoint | nodeId <- nodeId }
                      Nothing -> { activePoint | nodeId <- 0 }
            in
              { state |
                tree <- newTree5,
                activePoint <- newActivePoint,
                remainder <- tree.remainder - 1,
                lastSplitNode <- Just activeEdge.pointingTo }


-- Move `activePoint` onto the edge that starts with `char`
-- TODO handle walking off edge
apSetEdge : UkkonenTree -> ActivePoint-> Char -> Int -> ActivePoint
apSetEdge tree activePoint char labelStart =
  { activePoint | edge <- Just (char, labelStart) }


-- Convenience method for looking up the character at the given position in the
-- input string
getChar : Array Char -> Int -> Char
getChar str i = case Array.get i str of
  Just c -> c
  Nothing -> Debug.crash "Tried to look up character out of string bounds"


--
-- Tree Manipulation Methods
--

-- Get the edge that starts with `char`
getEdge : UkkonenTree -> NodeId -> Char -> Maybe UkkonenEdge
getEdge tree nodeId char = case IntDict.get nodeId tree of
  Just node -> Dict.get char node.edges
  Nothing -> Debug.crash "Active point is set to a node that doesn't exist"


-- Add `edge` that starts with `char`
addEdge : UkkonenTree ->
          NodeId ->
          NodeId ->
          Char ->
          Int ->
          ClosingIndex ->
          UkkonenTree
addEdge tree fromId toId char labelStart labelEnd = let
    node = getNode tree fromId
    newEdge = {
      pointingTo = toId,
      labelStart = labelStart,
      labelEnd = labelEnd }
    newEdges = Dict.insert char newEdge node.edges
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
    newNode = {edges = Dict.empty, suffixLink = Nothing}
    newTree = IntDict.insert count newNode tree
  in
    (newTree, count)


-- Set the suffix link of a node
setSuffixLink : UkkonenTree -> NodeId -> NodeId -> UkkonenTree
setSuffixLink tree fromId toId = let
    node = getNode tree fromId
  in
    IntDict.insert fromId { node | suffixLink <- Just toId } tree
