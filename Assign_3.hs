{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2021
-}
module Assign_3 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Michael Kim
-- Date: 19.11.2022
macid :: String
macid = "kim370"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
newtype Graph a = Graph [(Node a,[NodeID])]
  deriving (Show,Eq)

type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph = Graph [(nodeA,[1,2])
                ,(nodeB,[])
                ,(nodeC,[1,2])]

{- -----------------------------------------------------------------
 - maxNodeID
 - -----------------------------------------------------------------
- Description:
 -   This function finds the largest nodeID
 - -----------------------------------------------------------------
 - |   Input    |                                   
 - |   g        | Graph Input                             
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   Just nID | nodeID Output                               
 - -----------------------------------------------------------------
 -}
maxNodeID :: Graph a -> Maybe NodeID
maxNodeID (Graph []) = Nothing
maxNodeID (Graph nodes) = Just (maximum (map (getNodeID . fst ) nodes))


{- -----------------------------------------------------------------
 - insertNode
 - -----------------------------------------------------------------
- Description:
 -   This function inserts a new node associated with the Int
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   v        | Int Input                           
 - |   g        | Graph Input                             
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   g        | Graph Output                               
 - -----------------------------------------------------------------
 -}
 --had to be done like so because the previous function uses Just. And conversion is not viable
maxNodeNum :: Graph a -> NodeID
maxNodeNum (Graph nodes) = maximum (map (getNodeID . fst ) nodes)

insertNode :: a -> Graph a -> Graph a
insertNode v (Graph []) = Graph [(Node 0 v, [])]
insertNode v (Graph nodes) = Graph ((Node (maxNodeNum (Graph nodes) +1) v, []) : nodes)


{- -----------------------------------------------------------------
 - removeNode
 - -----------------------------------------------------------------
- Description:
 -   This function removes the node associated with the nodeID
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   nID      | NodeID Input                           
 - |   g        | Graph Input                             
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   g        | Graph output                               
 - -----------------------------------------------------------------
 -}
-- check for edges. Remove the corresponding ones. Delete the actual node after
nodeDel :: NodeID -> [(Node a,[NodeID])] -> [(Node a,[NodeID])]
nodeDel nID [] = []
nodeDel nID (x:xs)
  | getNodeID (fst x) == nID = nodeDel nID xs
  | otherwise = (fst x, filter (/=nID) (snd x) ) :nodeDel nID xs

removeNode :: NodeID -> Graph a -> Graph a
removeNode nID (Graph []) =  Graph []
removeNode nID (Graph nodes) = Graph (nodeDel nID nodes )


{- -----------------------------------------------------------------
 - lookupNode
 - -----------------------------------------------------------------
- Description:
 -   This function finds the node associated with the nodeID
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   nID      | NodeID Input                           
 - |   g        | Graph Input                             
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   Just node   | Just Node output                               
 - -----------------------------------------------------------------
 -}
lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID (Graph []) = Nothing
lookupNode nID (Graph (n:nodes))
  | getNodeID (fst n) == nID = Just (fst n)
  | otherwise = lookupNode nID (Graph nodes)


{- -----------------------------------------------------------------
 - insertEdge
 - -----------------------------------------------------------------
- Description:
 -   This function inserts the second nodeID into the first node's edges
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   (n1, n2) | NodeID Tuple Input                           
 - |   g        | Graph Input                             
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   Just g   | Just Graph output                               
 - -----------------------------------------------------------------
 -}
insertCheck :: Eq a => (NodeID,NodeID) -> [(Node a,[NodeID])] -> [(Node a,[NodeID])]
insertCheck (n1,n2) [] = []
insertCheck (n1,n2) (x:xs)
  | n2 `elem` snd x && (getNodeID (fst x) == n1) = x:xs
  | getNodeID (fst x) == n1 = (fst x, snd x ++ [n2]) : xs
  | otherwise = x : insertCheck (n1,n2) xs

insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _       (Graph [])  = Nothing
insertEdge (n1,n2) g@(Graph graph)
  | not containsBothNodes = Nothing
  | otherwise             = Just (Graph (insertCheck (n1,n2) graph))
  where
    containsBothNodes :: Bool
    containsBothNodes
      | ((lookupNode n1 g) /= Nothing) && ((lookupNode n2 g) /= Nothing) = True
      | otherwise = False


{-
Function: maxNodeID
Test Case Number: 1
Input: (Graph [( (Node 0 'D'),[1,2]), ( (Node 1 'u'),[]), ( (Node 2 'h'),[1,2])])
Expected Output: Just 2
Actual Output: Just 2

Function: maxNodeID
Test Case Number: 2
Input: (Graph [])
Expected Output: Nothing
Actual Output: Nothing

Function: maxNodeID
Test Case Number: 3
Input: (Graph [( (Node 0 'A'),[]), ( (Node 2 'B'),[]), ( (Node 65 'C'),[]), ( (Node 6 'D'),[])])
Expected Output: Just 65
Actual Output: Just 65



Function: insertNode
Test Case Number: 1
Input: 'T' (Graph [( (Node 0 'P'),[1,2,0]), ( (Node 1 'L'),[0]), ( (Node 2 'G'),[1,2,0])])
Expected Output: Graph [(Node {getNodeID = 3, getNodeVal = 'T'},[]),(Node {getNodeID = 0, getNodeVal = 'P'},[1,2,0]),(Node {getNodeID = 1, getNodeVal = 'L'},[0]),(Node {getNodeID = 2, getNodeVal = 'G'},[1,2,0])]
Actual Output: Graph [(Node {getNodeID = 3, getNodeVal = 'T'},[]),(Node {getNodeID = 0, getNodeVal = 'P'},[1,2,0]),(Node {getNodeID = 1, getNodeVal = 'L'},[0]),(Node {getNodeID = 2, getNodeVal = 'G'},[1,2,0])]

Function: insertNode
Test Case Number: 2
Input: 'W' (Graph [( (Node 0 'G'),[])])
Expected Output: Graph [(Node {getNodeID = 1, getNodeVal = 'W'},[]),(Node {getNodeID = 0, getNodeVal = 'G'},[])]
Actual Output: Graph [(Node {getNodeID = 1, getNodeVal = 'W'},[]),(Node {getNodeID = 0, getNodeVal = 'G'},[])]

Function: insertNode
Test Case Number: 3
Input: 'A' (Graph [])
Expected Output: Graph [(Node {getNodeID = 0, getNodeVal = 'A'},[])]
Actual Output: Graph [(Node {getNodeID = 0, getNodeVal = 'A'},[])]



Function: removeNode
Test Case Number: 1
Input: 2 (Graph [( (Node 5 'D'),[2]), ( (Node 1 'M'),[]), ( (Node 2 'N'),[5,2])])
Expected Output: Graph [(Node {getNodeID = 5, getNodeVal = 'D'},[]),(Node {getNodeID = 1, getNodeVal = 'M'},[])]
Actual Output: Graph [(Node {getNodeID = 5, getNodeVal = 'D'},[]),(Node {getNodeID = 1, getNodeVal = 'M'},[])]

Function: removeNode
Test Case Number: 2
Input: 4 (Graph [( (Node 2 'N'),[2])])
Expected Output: Graph [(Node {getNodeID = 2, getNodeVal = 'N'},[2])]
Actual Output: Graph [(Node {getNodeID = 2, getNodeVal = 'N'},[2])]

Function: removeNode
Test Case Number: 3
Input: 1 (Graph [])
Expected Output: Graph []
Actual Output: Graph []



Function: lookupNode
Test Case Number: 1
Input: 2 (Graph [( (Node 6 'D'),[2]), ( (Node 8 'M'),[8,2]), ( (Node 2 'N'),[6,2])])
Expected Output: Just (Node {getNodeID = 2, getNodeVal = 'N'})
Actual Output: Just (Node {getNodeID = 2, getNodeVal = 'N'})

Function: lookupNode
Test Case Number: 2
Input: 10 (Graph [( (Node 8 'H'),[2]), ( (Node 64 'M'),[8,64]), ( (Node 2 'N'),[64,2]), ( (Node 10 'Y'),[10,8,64])])
Expected Output: Just (Node {getNodeID = 10, getNodeVal = 'Y'})
Actual Output: Just (Node {getNodeID = 10, getNodeVal = 'Y'})

Function: lookupNode
Test Case Number: 3
Input: 100 (Graph [( (Node 20 'Z'),[])])
Expected Output: Nothing
Actual Output: Nothing



Function: insertEdge
Test Case Number: 1
Input: (76,2) (Graph [( (Node 45 'T'),[2]), ( (Node 76 'Y'),[45,76]), ( (Node 2 'W'),[])])
Expected Output: Just (Graph [(Node {getNodeID = 45, getNodeVal = 'T'},[2]),(Node {getNodeID = 76, getNodeVal = 'Y'},[45,76,2]),(Node {getNodeID = 2, getNodeVal = 'W'},[])])
Actual Output: Just (Graph [(Node {getNodeID = 45, getNodeVal = 'T'},[2]),(Node {getNodeID = 76, getNodeVal = 'Y'},[45,76,2]),(Node {getNodeID = 2, getNodeVal = 'W'},[])])

Function: insertEdge
Test Case Number: 2
Input: (21,2) (Graph [( (Node 21 'H'),[0]), ( (Node 98 'E'),[98,0]), ( (Node 0 'A'),[0,98])])
Expected Output: Nothing
Actual Output: Nothing

Function: insertEdge
Test Case Number: 3
Input: (0,0) (Graph [])
Expected Output: Nothing
Actual Output: Nothing

-}