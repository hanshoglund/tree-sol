/* jshint strict:false, asi: true, esversion: 6, loopfunc:true */
/* globals console */

const solution = function(t) {
  return distFromCapital(t)
}

const makeAssocs = a  => a.map((e,i)  => [i, e])
const catMaybes  = xs => Array.prototype.concat.apply([], xs)

const contains = function(obj, arr) {
    let i = arr.length;
    while (i--) {
        if (arr[i] == obj) {
            return true;
        }
    }
    return false;
}

const treeFromAssoc = function(xs) {
    const assocs = makeAssocs(xs)
    let root = null;
    assocs.forEach(function([x, y]) {
      if (x === y) {
        root = x;
      }
    })
    if (root === null) {
      throw("No root")
    }
    return treeFromNode(assocs, [root], root)
}

const treeFromNode = function (assocs, visited, n) {
  const [visited2, xs] = childrenOf(assocs, n, visited)
  if (xs.length === 0) {
    return {type: "leaf"}
  } else {
    return {type: "branch", children :xs.map(ys => treeFromNode(assocs, visited2, ys)) }
  }
}

const childrenOf = function (assocs, node, visited) {
  const children = catMaybes(assocs.map(function([x, y]) {
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

const nodesAtDistance = function (t) {
  if (t.type === "leaf") {
    return []
  }
  if (t.type === "branch") {
    return nodesAtDistanceInclRoot(t).slice(1)
  }

}

const nodesAtDistanceInclRoot = function (t) {
  if (t.type === "leaf") {
    return [1]
  }
  if (t.type === "branch") {
    return [1].concat(sumEachLevel(t.children.map(nodesAtDistanceInclRoot)))
  }
}

const sumEachLevel = function (xxs) {
  const maxLength = xxs.map(xs => xs.length).reduce((x,y) => Math.max(x,y), 0)
  let rs = []
  for (let i = 0; i < maxLength; ++i) {
    let r = 0
    xxs.forEach(function(xs) {
      r += (xs[i]|0)
    })
    rs.push(r)
  }
  return rs
}

const padWithZeroToLength = function(n,xs) {
  return xs.concat(new Array(n - xs.length).fill(0))
}

const distFromCapital = function(spec) {
  return padWithZeroToLength(spec.length - 1, nodesAtDistance(treeFromAssoc(spec)))
}


// Test

console.log(
    JSON.stringify(distFromCapital([9, 1, 4, 9, 0, 4, 8, 9, 0, 1])) ===
  		JSON.stringify([1,3,2,3,0,0,0,0,0])
      )
console.log(
    JSON.stringify(distFromCapital([0,0,0,2])) ===
      JSON.stringify([2,1,0])
      )
console.log(
    JSON.stringify(distFromCapital([1,1,1,0,1])) ===
      JSON.stringify([3,1,0,0])
      )
console.log(
    JSON.stringify(distFromCapital([1,5,1,4,5,5,0])) ===
      JSON.stringify([2,3,1,0,0,0])
      )
console.log(
    JSON.stringify(distFromCapital([0])) ===
  		JSON.stringify([])
      )
