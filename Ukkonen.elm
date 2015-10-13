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
    state = { state | string <- push char tree.string }

    -- Get convenient references to the tree record's fields
    {tree, remainder, activePoint, string} = state

    -- Get the index of the character being inserted
    i = Array.length string - 1
  in
    -- If the active point is an explicit suffix, i.e. there's no active edge
    case activePoint.edge of
      Nothing ->

        -- If an edge starting with the character already exists at this node
        case getEdge tree activePoint.nodeId char of
          Just edge -> { state |
            activePoint <- apSetEdge tree activePoint char 1,
            remainder <- tree.remainder + 1 }

          Nothing -> let
              (newTree, newId) = addNode tree
            in
              { state |
                tree <- addEdge newTree activePoint.nodeId newId char i,
                activePoint <- apSetEdge tree activePoint char 1 }

      Just (edgeChar, edgeSteps) ->
        case getEdge tree activePoint.nodeId edgeChar of
          Just edge -> 
            let
              -- This is the index of the input string that the current edge
              -- location points to.
              currentStringIndex = edge.from + edgeSteps
            in
              -- If the new suffix is already implicitly present in the tree
              case Array.get currentStringIndex tree.string of
                Just c ->
                  if char == c then
                    { tree |
                      activePoint 
                        <- apSetEdge tree activePoint edgeChar edgeSteps + 1,
                      remainder <- tree.remainder + 1 }
                  else tree
                    --{ tree |
                    --  activePoint <- apSplitEdge activePoint (edge, edgeSteps),
                    --  remainder <- tree.remainder - 1 }
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
addEdge : UkkonenTree -> NodeId -> NodeId -> Char -> Int -> UkkonenTree
addEdge tree fromId toId char labelStart = let
    node = getNode tree fromId
    newEdges = Dict.insert char (unboundEdge toId labelStart) node.edges
    newNode = {node | edges <- newEdges}
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
    (IntDict.insert size emptyNode tree count)

-- Creates a new empty node
emptyNode : UkkonenNode
emptyNode = {edges = Dict.empty, suffixLink = Nothing}


-- Creates a new unbound edge
unboundEdge : NodeId -> Int -> UkkonenEdge
unboundEdge nodeId labelStart = {
  pointingTo = nodeId,
  labelStart = labelStart,
  labelEnd = EndOfString }


-- Create a new, unbound edge from `activePoint` that starts with `char`
apCreateEdge : ActivePoint -> Int -> Char -> ActivePoint
apCreateEdge activePoint i char = let
    newEdge = unboundEdge i
  in
    { activePoint | node <- addEdge activePoint.node char newEdge }


-- Move `activePoint` onto the edge that starts with `char`
apSetEdge : UkkonenTree -> ActivePoint-> Char -> Int -> ActivePoint
apSetEdge tree activePoint char labelStart =
  { activePoint | edge <- Just (char, labelStart) }
