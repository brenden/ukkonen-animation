import Dict exposing (..)
import Array exposing (..)
import String exposing (..)

type alias UkkonenTree = Dict NodeId UkkonenNode

type alias UkkonenNode = {
  edges:Dict Char UkkonenEdge
  suffixLink:Maybe NodeId}

type alias UkkonenEdge = { 
  pointingTo:NodeID,
  labelStart:Int, 
  labelEnd:ClosingIndex }

type alias ActivePoint = {
  node:UkkonenNode,
  edge:Maybe (UkkonenEdge, Int) }

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
        case getEdge activePoint.node char of
          Just edge -> { tree |
            activePoint <- apAdvanceToEdge activePoint edge,
            remainder <- tree.remainder + 1 }

          Nothing -> { tree |
            activePoint <- apCreateEdge activePoint i char }

      Just (edge, edgeSteps) ->
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
                  activePoint <- apAdvanceOnEdge activePoint (edge, edgeSteps),
                  remainder <- tree.remainder + 1 }
              else
                { tree |
                  activePoint <- apSplitEdge activePoint (edge, edgeSteps),
                  remainder <- tree.remainder - 1 }
            Nothing -> tree


-- Check whether the node has an edge that starts with `char`
hasEdge : UkkonenNode -> Char -> Bool
hasEdge (UkkonenNode edges _) char = member char edges


-- Get the edge that starts with `char`
getEdge : UkkonenNode -> Char -> Maybe UkkonenEdge
getEdge (UkkonenNode edges _) char = Dict.get char edges


-- Add `edge` that starts with `char`
addEdge : UkkonenNode -> Char -> UkkonenEdge -> UkkonenNode
addEdge (UkkonenNode edges suffixLink) char edge = UkkonenNode (Dict.insert char
  edge edges) suffixLink


-- Creates a new empty node
emptyNode : UkkonenNode
emptyNode = UkkonenNode (Dict.empty) Nothing


-- Creates a new unbound edge
unboundEdge : Int -> UkkonenEdge
unboundEdge fromIndex = {
  pointingTo = emptyNode,
  from = fromIndex,
  to = CurrentEnd }


-- Create a new, unbound edge from `activePoint` that starts with `char`
apCreateEdge : ActivePoint -> Int -> Char -> ActivePoint
apCreateEdge activePoint i char = let
    newEdge = unboundEdge i
  in
    { activePoint | node <- addEdge activePoint.node char newEdge }


-- Move `activePoint` onto the edge that starts with `char`
apAdvanceToEdge : ActivePoint-> UkkonenEdge -> ActivePoint
apAdvanceToEdge activePoint newEdge = 
  { activePoint | edge <- Just (newEdge, 1) }


-- Move another step on the active edge (possibly moving off of it onto a node)
apAdvanceOnEdge : ActivePoint -> (UkkonenEdge, Int) -> ActivePoint
apAdvanceOnEdge activePoint (edge, edgeSteps) =
  case edge.to of
    CurrentEnd -> { activePoint | edge <- Just (edge, edgeSteps + 1) }
    Definite to ->
      if (to - edge.from) - 1 == edgeSteps then
        { activePoint | 
          node <- edge.pointingTo,
          edge <- Just (edge, 0) }
      else
        { activePoint | edge <- Just (edge, edgeSteps + 1) }


-- Split the current edge at the shared prefix, resulting in two new nodes
apSplitEdge : ActivePoint -> (UkkonenEdge, Int) -> ActivePoint
apSplitEdge activePoint (edge, edgeSteps) = activePoint
