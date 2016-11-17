{-# LANGUAGE ScopedTypeVariables#-}

{-# OPTIONS_GHC
  -fwarn-incomplete-patterns
  -fno-warn-name-shadowing
  -fno-warn-unused-binds
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  -fno-warn-type-defaults
  -fno-warn-missing-signatures
  -Werror
  #-}
module Main where
import BasePrelude hiding ((.))

-- import Data.Tree Int

main :: IO ()
main = do
  putStrLn "hello world 2"

data Tree a = Leaf a | Branch a [Tree a]
  deriving (Eq, Ord, Show)


-- node-node assocs, assoc to itself means root
treeFromAssoc :: [Int] -> Tree Int
treeFromAssoc xs = foo [root] root
  where
    foo :: [Int] -> Int -> Tree Int
    foo visited n = case childrenOf n visited of
      (visited2, []) -> Leaf n
      (visited2, xs) -> Branch n $ fmap (foo visited2) xs

    -- with given element as root, return ALL child nodes, or [] if this is a leaf
    -- based on assocs
    -- returns (visitedNodes, children)
    childrenOf :: Int -> [Int] -> ([Int], [Int])
    childrenOf node visited  = (visited ++ children, children)
      where
        -- removeAll these from = filter (\x -> not $ x `elem` these) from

        children :: [Int]
        children = catMaybes $ fmap g assocs
          where
            g (x,y)
              | (x == node && not (elem y visited)) = Just y
              | (y == node && not (elem x visited)) = Just x
              | otherwise = Nothing
        -- assocsInvolvingNode :: Int -> [Maybe Int]
        -- assocsInvolvingNode n =

    root :: Int
    root = fromMaybe (error "no root") $ fmap fst $ listToMaybe $ filter (\(x,y) -> x == y) assocs
    -- a list of edges
    -- NOTE each edge appear only once and may be in any dir
    assocs = zip [0..] xs



-- Given a tree, return number of nodes at each depth/distance.
-- in returned list, first element is one level below root node etc
nodesAtDistance :: Tree Int -> [Int]
nodesAtDistance (Leaf _) = error "can  not handle empty tree"
nodesAtDistance t@_      = tail $ nodesAtDistanceInclRoot t
  where
    -- in returned list, root node is level 0 (1st element)
    nodesAtDistanceInclRoot :: Tree Int -> [Int]
    nodesAtDistanceInclRoot (Leaf _)      = [1]
    nodesAtDistanceInclRoot (Branch _ xs) = 1 : (sumEachLevel $ fmap nodesAtDistanceInclRoot xs)

sumEachLevel :: [[Int]] -> [Int]
sumEachLevel = fmap sum . transpose

{-
argument is an assoc list (indices 1st pair element), each assoc is an edge between nodes
an edge to itself (identity) indicates root node ("capital")

----
return a Map (Distance ~ Int) to (Count ~ Int), count is number of cities at this distance
return map up to (argument.length - 1), i.e. given 10 arguments, return an array of 9 elements
  i.e. never count elements at distance 0

----
EXAMPLE CASE I
  [9, 1, 4, 9, 0, 4, 8, 9, 0, 1]
  [1,3,2,3,0,0,0,0,0]
CUSTOM CASE I
-}
distFromCapital :: [Int] -> [Int]
distFromCapital spec = nodesAtDistance (treeFromAssoc spec)

runTests = do
  print $ distFromCapital [9, 1, 4, 9, 0, 4, 8, 9, 0, 1] ==   [1,3,2,3,0,0,0,0,0]
  -- print $ distFromCapital [0] ==   []
  print $ distFromCapital [0,0,0,2] ==   [2,1,0]
  print $ distFromCapital [1,1,1,0,1] ==   [3,1,0,0]


-- testNAD = nodesAtDistance $ Branch [Leaf 1, Branch [Leaf 3]]
