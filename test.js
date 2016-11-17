
"use strict"

function solution(t) {
  distFromCapital(t)
}


var makeLeaf   = x      => ["Leaf", x]
var makeBranch = (x,xs) => ["Branch", x, xs]
var zip        = (a, b) => a.map((e, i) => [e, b[i]])
var makeAssocs = (a)    => a.map((e,i)  => [i, e])
var catMaybes  = xs => Array.prototype.concat.apply([], xs)

Array.prototype.contains = function(obj) {
    var i = this.length;
    while (i--) {
        if (this[i] == obj) {
            return true;
        }
    }
    return false;
}
var transpose = function(a) {

  // Calculate the width and height of the Array
  var w = a.length ? a.length : 0,
    h = a[0] instanceof Array ? a[0].length : 0;

  // In case it is a zero matrix, no transpose routine needed.
  if(h === 0 || w === 0) { return []; }

  /**
   * @var {Number} i Counter
   * @var {Number} j Counter
   * @var {Array} t Transposed data is stored in this array.
   */
  var i, j, t = [];

  // Loop through every item in the outer array (height)
  for(i=0; i<h; i++) {

    // Insert a new row (array)
    t[i] = [];

    // Loop through every item per item in outer array (width)
    for(j=0; j<w; j++) {

      // Save transposed data.
      t[i][j] = a[j][i];
    }
  }
  return t;
};

var treeFromAssoc = function(xs) {
    let assocs = makeAssocs(xs)
    let root = null;
    assocs.forEach(function(xy) {
      // console.log("xs", xs)
      var x = xy[0]
      var y = xy[1]
      if (x === y) {
        root = x;
      }
    })
    // console.log(assocs, root)
    if (root === null) {
      throw("No root")
    }
    return treeFromNode(assocs, [root], root)
}

function treeFromNode(assocs, visited, n) {
  // console.log("enter treeFromNode", assocs, visited, n)
  let res = childrenOf(assocs, n, visited)
  let visited2 = res[0]
  let xs       = res[1]
  if (xs.length == 0) {
    return makeLeaf(n)
  } else {
    return makeBranch(n, xs.map(ys => treeFromNode(assocs, visited2, ys)))
  }
}

function childrenOf(assocs, node, visited) {
  // console.log("enter childrenOf", assocs)
  let children = catMaybes(assocs.map(function(xy) {
    var x = xy[0]
    var y = xy[1]
    if ((x === node) && !(visited.contains(y))) {
      return [y]
    }
    if ((y === node) && !(visited.contains(x))) {
      return [x]
    }
    return []
  }))
  // console.log("    return childrenOf", children)
  return [visited.concat(children), children]
}


var nodesAtDistance = function (t) {
  if (t[0] === "Leaf") {
    return []
  }
  if (t[0] === "Branch") {
    return nodesAtDistanceInclRoot(t).slice(1)
  }
}

function nodesAtDistanceInclRoot(t) {
  if (t[0] === "Leaf") {
    return [1]
  }
  if (t[0] === "Branch") {
    let xs = t[2]
    return [1].concat(sumEachLevel(xs.map(nodesAtDistanceInclRoot)))
  }
}

function sumEachLevel(xs) {
  return transpose(xs).map(ys => ys.reduce((x, y) => (x|0) + (y|0), 0))
}

var distFromCapital = function(spec) {
  return padWithZeroToLength(spec.length - 1, nodesAtDistance(treeFromAssoc(spec)))
}

function padWithZeroToLength(n,xs) {
  return xs.concat(new Array(n - xs.length).fill(0))
}



// console.log(transpose([[1,2,1],[0]]))
// console.log(sumEachLevel([[1,2,1],[10]]))
// console.log(distFromCapital([9, 1, 4, 9, 0, 4, 8, 9, 0, 1]).map(x => typeof(x)) )
// console.log(distFromCapital([9, 1, 4, 9, 0, 4, 8, 9, 0, 1]) )

// module.exports = distFromCapital

console.log(
    JSON.stringify(distFromCapital([9, 1, 4, 9, 0, 4, 8, 9, 0, 1]))
      )
console.log(
    JSON.stringify(distFromCapital([0,0,0,2]))
      )
console.log(
    JSON.stringify(distFromCapital([1,1,1,0,1]))
      )


console.log(
    JSON.stringify(distFromCapital([9, 1, 4, 9, 0, 4, 8, 9, 0, 1]))
      ===
      JSON.stringify([1,3,2,3,0,0,0,0,0])
      )
console.log(
    JSON.stringify(distFromCapital([0,0,0,2]))
      ===
      JSON.stringify(distFromCapital([2,1,0]))
      )
console.log(
    JSON.stringify(distFromCapital([1,1,1,0,1]))
      ===
      JSON.stringify([3,1,0,0])
      )


// console.log(
//
//     makeAssocs([4,4])
//
//   )



/*
HASKELL SOLUTION

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
main = runTests

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
nodesAtDistance (Leaf _) = []
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
distFromCapital spec = padWithZeroToLength (length spec - 1) $ nodesAtDistance (treeFromAssoc spec)
  where
padWithZeroToLength n xs = take n $ xs ++ repeat 0

runTests = do
  print $ distFromCapital [9, 1, 4, 9, 0, 4, 8, 9, 0, 1] ==  [1,3,2,3,0,0,0,0,0]
  -- print $ distFromCapital [0] ==   []
  print $ distFromCapital [0,0,0,2] ==   [2,1,0]
  print $ distFromCapital [1,1,1,0,1] ==   [3,1,0,0]

  print $ not $ distFromCapital [1,1,1,0,1] ==   [3,1,0,0,5]
  print $ not $ distFromCapital [1,1,1,0,1] == []
  print $ not $ distFromCapital [9, 1, 4, 9, 0, 4, 8, 9, 5, 1] ==  [1,3,2,3,0,0,0,0,0]

-- testNAD = nodesAtDistance $ Branch [Leaf 1, Branch [Leaf 3]]

*/
