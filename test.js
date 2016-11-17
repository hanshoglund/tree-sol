
"use strict"

function solution(t) {
  return distFromCapital(t)
}

var makeLeaf   = x      => ["Leaf", x]
var makeBranch = (x,xs) => ["Branch", x, xs]
var zip        = (a, b) => a.map((e, i) => [e, b[i]])
var makeAssocs = (a)    => a.map((e,i)  => [i, e])
var catMaybes  = xs => Array.prototype.concat.apply([], xs)

var contains = function(obj, arr) {
    var i = arr.length;
    while (i--) {
        if (arr[i] == obj) {
            return true;
        }
    }
    return false;
}

var treeFromAssoc = function(xs) {
    let assocs = makeAssocs(xs)
    let root = null;
    assocs.forEach(function(xy) {
      var x = xy[0]
      var y = xy[1]
      if (x === y) {
        root = x;
      }
    })
    if (root === null) {
      throw("No root")
    }
    return treeFromNode(assocs, [root], root)
}

var treeFromNode = function (assocs, visited, n) {
  let res = childrenOf(assocs, n, visited)
  let visited2 = res[0]
  let xs       = res[1]
  if (xs.length == 0) {
    return makeLeaf(n)
  } else {
    return makeBranch(n, xs.map(ys => treeFromNode(assocs, visited2, ys)))
  }
}

var childrenOf = function (assocs, node, visited) {
  let children = catMaybes(assocs.map(function(xy) {
    var x = xy[0]
    var y = xy[1]
    if ((x === node) && !(contains(y, visited))) {
      return [y]
    }
    if ((y === node) && !(contains(x, visited))) {
      return [x]
    }
    return []
  }))
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

var nodesAtDistanceInclRoot = function (t) {
  if (t[0] === "Leaf") {
    return [1]
  }
  if (t[0] === "Branch") {
    let xs = t[2]
    return [1].concat(sumEachLevel(xs.map(nodesAtDistanceInclRoot)))
  }
}

var sumEachLevel = function (xxs) {
  let maxLength = xxs.map(xs => xs.length).reduce((x,y) => Math.max(x,y), 0)
  var rs = []
  for (var i = 0; i < maxLength; ++i) {
    var r = 0
    xxs.forEach(function(xs) {
      r += (xs[i]|0)
    })
    rs.push(r)
  }
  return rs
}

var distFromCapital = function(spec) {
  return padWithZeroToLength(spec.length - 1, nodesAtDistance(treeFromAssoc(spec)))
}

var padWithZeroToLength = function(n,xs) {
  return xs.concat(new Array(n - xs.length).fill(0))
}


// Test

console.log(
    JSON.stringify(distFromCapital([9, 1, 4, 9, 0, 4, 8, 9, 0, 1]))
      ===
      JSON.stringify([1,3,2,3,0,0,0,0,0])
      )
console.log(
    JSON.stringify(distFromCapital([0,0,0,2]))
      ===
      JSON.stringify([2,1,0])
      )
console.log(
    JSON.stringify(distFromCapital([1,1,1,0,1]))
      ===
      JSON.stringify([3,1,0,0])
      )
console.log(
    JSON.stringify(distFromCapital([1,5,1,4,5,5,0]))
      ===
      JSON.stringify([2,3,1,0,0,0])
      )
