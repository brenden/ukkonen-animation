import Dict exposing (..)
import Array exposing (..)

type UkkonenNode = UkkonenNode (Dict Char UkkonenEdge) (Maybe UkkonenNode)

type ClosingIndex = Definite Int | CurrentEnd

type alias UkkonenEdge = { 
  pointingTo:UkkonenNode, 
  from:Int, 
  to:ClosingIndex }

type alias ActivePoint = {
  node:UkkonenNode,
  edge:Maybe UkkonenEdge,
  edgeSteps:Int }

type alias UkkonenTree = {
  root:UkkonenNode,
  remainder:Int,
  activePoint:ActivePoint,
  string:Array Char }

-- Check whether the node has an edge that starts with `char`
hasEdge : UkkonenNode -> Char -> Bool
hasEdge (UkkonenNode edges _) char = member char edges

-- Get the edge going from `node` that starts with `char`
getEdge : UkkonenNode -> Char -> Maybe UkkonenEdge
getEdge (UkkonenNode edges _) char = Dict.get char edges

-- Move `activePoint` onto the edge that starts with `char`
advanceToEdge : UkkonenTree -> Char -> Maybe UkkonenTree
advanceToEdge tree char = case getEdge tree.activePoint.node char of
  Just newEdge -> let
      activePoint = tree.activePoint
      newActivePoint = { activePoint | edge <- Just newEdge, edgeSteps <- 1 }
    in 
      Just { tree | activePoint <- newActivePoint } 
  Nothing -> Nothing

-- Add another character to the tree
insert : UkkonenTree -> Char -> Maybe UkkonenTree
insert tree char =
  let
    {root, remainder, activePoint, string} = tree
  in
    -- if the active point is an explicit suffix, i.e. there's no active edge
    case activePoint.edge of
      Nothing ->

        -- if an edge starting with the character already exists at this node
        if hasEdge activePoint.node char then
          advanceToEdge tree char 
        else
          Just tree

      Just edge -> 
        let
          activeEdgeLength = case edge.to of
            CurrentEnd -> CurrentEnd
            Definite to -> Definite (to - edge.from)

          -- This is the index of the input string that the current edge
          -- location points to.
          currentStringIndex = edge.from + activePoint.edgeSteps 
        in
          -- if the new suffix is already implicitly present in the tree
          case Array.get currentStringIndex tree.string of
            Just c -> 
              if char == c then
                Just tree
              else
                Just tree
            Nothing -> Nothing
