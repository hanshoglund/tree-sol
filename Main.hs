{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

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


main :: IO ()
main = runTests

data Tree a = Leaf a | Branch a [Tree a]
  deriving (Eq, Ord, Show)

-- node-node assocs, assoc to itself means root
treeFromAssoc :: [Int] -> Tree Int
treeFromAssoc xs = treeFromNode assocs [root] root
  where
    -- a list of edges
    -- NOTE each edge appear only once and may be in any dir
    assocs = zip [0..] xs
    root :: Int
    root = fromMaybe (error "no root") $ fmap fst $ listToMaybe $ filter (\(x,y) -> x == y) assocs

treeFromNode :: [(Int,Int)] -> [Int] -> Int -> Tree Int
treeFromNode assocs visited n = case childrenOf assocs n visited of
  (visited2, []) -> Leaf n
  (visited2, xs) -> Branch n $ fmap (treeFromNode assocs visited2) xs

-- with given element as root, return ALL child nodes, or [] if this is a leaf
childrenOf :: [(Int, Int)] -> Int -> [Int] -> ([Int], [Int])
childrenOf assocs node visited  = (visited ++ children, children)
  where
    children :: [Int]
    children = catMaybes $ fmap g assocs
      where
        g (x,y)
          | (x == node && not (elem y visited)) = Just y
          | (y == node && not (elem x visited)) = Just x
          | otherwise = Nothing

-- Given a tree, return number of nodes at each depth/distance.
-- in returned list, first element is one level below root node etc
nodesAtDistance :: Tree Int -> [Int]
nodesAtDistance (Leaf _) = []
nodesAtDistance t@_      = tail $ nodesAtDistanceInclRoot t
  where
    -- in returned list, root node is level 0 (1st element)
    nodesAtDistanceInclRoot :: Tree Int -> [Int]
    nodesAtDistanceInclRoot (Leaf _)      = [1]
    nodesAtDistanceInclRoot (Branch _ xs) = 1 : (sumEachLevel $ fmap nodesAtDistanceInclRoot xs)

sumEachLevel :: [[Int]] -> [Int]
sumEachLevel = fmap sum . transpose

distFromCapital :: [Int] -> [Int]
distFromCapital spec = tx $ padWithZeroToLength (length spec - 1) $ nodesAtDistance (treeFromAssoc spec)
  where
padWithZeroToLength n xs = take n $ xs ++ repeat 0

-- Debug
tx x = x
-- tx = (\x -> trace ("Tr "<> show x) x)


runTests = do
  -- print $ distFromCapital [0] ==   []
  print $ distFromCapital [9, 1, 4, 9, 0, 4, 8, 9, 0, 1] ==  [1,3,2,3,0,0,0,0,0]
  print $ distFromCapital [0,0,0,2] ==   [2,1,0]
  print $ distFromCapital [1,1,1,0,1] ==   [3,1,0,0]
  print $ distFromCapital [1,5,1,4,5,5,0] ==   [2,3,1,0,0,0]

  -- print $ not $ distFromCapital [1,1,1,0,1] ==   [3,1,0,0,5]
  -- print $ not $ distFromCapital [1,1,1,0,1] == []
  -- print $ not $ distFromCapital [9, 1, 4, 9, 0, 4, 8, 9, 5, 1] ==  [1,3,2,3,0,0,0,0,0]
