import Dict exposing (..)
import Array exposing (..)
import String exposing (..)

type UkkonenNode = UkkonenNode (Dict Char UkkonenEdge) (Maybe UkkonenNode)

type ClosingIndex = Definite Int | CurrentEnd

type alias UkkonenEdge = { 
  pointingTo:UkkonenNode, 
  from:Int, 
  to:ClosingIndex }

type alias ActivePoint = {
  node:UkkonenNode,
  edge:Maybe (UkkonenEdge, Int) }

type alias UkkonenTree = {
  root:UkkonenNode,
  remainder:Int,
  activePoint:ActivePoint,
  string:Array Char }


-- Add another character to the tree
insert : UkkonenTree -> Char -> Maybe UkkonenTree
insert tree char =
  let
    -- Append the new character to the tree's input string
    tree = { tree | string <- push char tree.string }

    -- Get convenient references to the tree record's fields
    {root, remainder, activePoint, string} = tree

    -- Get the index of the character being inserted
    i = Array.length string - 1
  in
    -- If the active point is an explicit suffix, i.e. there's no active edge
    case activePoint.edge of
      Nothing ->

        -- If an edge starting with the character already exists at this node
        if hasEdge activePoint.node char then
          apAdvanceToEdge tree char 
        else
          apCreateEdge tree i char

      Just edge -> 
        let
          -- This is the index of the input string that the current edge
          -- location points to.
          currentStringIndex = edge.from + activePoint.edgeSteps 
        in
          -- If the new suffix is already implicitly present in the tree
          case Array.get currentStringIndex tree.string of
            Just c -> 
              if char == c then
                apAdvanceOnEdge tree
              else
                Just tree
            Nothing -> Nothing


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
  { activePoint | edge <- Just newEdge, edgeSteps <- 1 }


-- Move another step on the active edge (possibly moving off of it onto a node)
apAdvanceOnEdge : ActivePoint -> ActivePoint
apAdvanceOnEdge activePoint =
  case activePoint.edge.to of
    CurrentEnd -> { activePoint | edgeSteps <- activePoint.edgeSteps + 1 }
    Definite to ->
      if (to - activePoint.edge.from) - 1 == activePoint.edgeSteps then
        { activePoint | 
          node <- activePoint.edge.pointingTo,
          edgeSteps <- 0 }
      else
        { activePoint | edgeSteps <- activePoint.edgeSteps + 1 }
