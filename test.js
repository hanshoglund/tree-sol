
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

function sumEachLevel(xxs) {
  // return transpose(xs).map(ys => ys.reduce((x, y) => (x|0) + (y|0), 0))
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
  return tx(padWithZeroToLength(spec.length - 1, nodesAtDistance(treeFromAssoc(spec))))
}

function padWithZeroToLength(n,xs) {
  return xs.concat(new Array(n - xs.length).fill(0))
}


function tx(x) {
  // console.log("Tr", x)
  return x
}

// console.log("MUST BE", sumEachLevel([ [ 1 ], [ 1, 1 ] ]))
// console.log(transpose([[1,2,1],[0]]))
// console.log(sumEachLevel([[1,2,1],[10]]))
// console.log(distFromCapital([9, 1, 4, 9, 0, 4, 8, 9, 0, 1]).map(x => typeof(x)) )
// console.log(distFromCapital([9, 1, 4, 9, 0, 4, 8, 9, 0, 1]) )

// module.exports = distFromCapital

// console.log(
//     JSON.stringify(distFromCapital([9, 1, 4, 9, 0, 4, 8, 9, 0, 1]))
//       )
// console.log(
//     JSON.stringify(distFromCapital([0,0,0,2]))
//       )
// console.log(
//     JSON.stringify(distFromCapital([1,1,1,0,1]))
//       )


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
