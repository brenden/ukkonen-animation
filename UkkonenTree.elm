module UkkonenTree where

import IntDict exposing (..)
import Dict exposing (..)
import String exposing (..)
import Debug

type alias NodeId = Int
type alias UkkonenTree = IntDict UkkonenNode

type alias UkkonenNode = {
  edges:Dict Char UkkonenEdge,
  suffixLink:Maybe NodeId }

type alias UkkonenEdge = {
  pointingTo:NodeId,
  labelStart:Int,
  labelEnd:ClosingIndex }

type ClosingIndex = Definite Int | EndOfString


-- Create an empty tree
emptyTree : UkkonenTree
emptyTree = let
    newNode = {edges = Dict.empty, suffixLink = Nothing}
  in
    IntDict.insert 0 newNode IntDict.empty


-- Get the edge that starts with `char`
getEdge : UkkonenTree -> NodeId -> Char -> Maybe UkkonenEdge
getEdge tree nodeId char = case IntDict.get nodeId tree of
  Just node -> Dict.get char node.edges
  Nothing -> Debug.crash "Active point is set to a node that doesn't exist"


-- Add `edge` that starts with `char`
setEdge : UkkonenTree ->
          NodeId ->
          NodeId ->
          Char ->
          Int ->
          ClosingIndex ->
          UkkonenTree
setEdge tree fromId toId char labelStart labelEnd = let
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


-- Prints out a representation of the tree
toString : UkkonenTree -> String
toString = toString' 0 0

toString' level rootId tree = let
    root = getNode tree rootId
    spaces = (String.repeat level "  ")
  in
    spaces ++ (Basics.toString rootId) ++
      newLine ++
        (String.concat (Dict.values
          (Dict.map ( \edgeLabel -> \edge ->
            spaces ++ (Basics.toString edgeLabel) ++ "->" ++ newLine ++
              (toString' (level + 1)
                         edge.pointingTo
                         tree))
                    root.edges)))


-- Convenince method for bulding strings that contain newlines
newLine = """
"""
