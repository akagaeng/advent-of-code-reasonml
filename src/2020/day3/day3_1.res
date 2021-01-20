// array of array

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(arr => arr->Js.String2.split(""))

let getOffset = (c, len) => {
  let idxMax = len - 1
  if c > idxMax {
    let q = c / len
    c - q * len
  } else {
    c
  }
}

let getTrees = (treeInputs, (slopeX, slopeY)) => {
  treeInputs
  ->Belt.Array.keepWithIndex((_, i) => mod(i, slopeY) === 0)
  ->Belt.Array.mapWithIndex((i, treeMap) => {
    let treeMapLen = Belt.Array.length(treeMap)
    let offset = getOffset(i * slopeX, treeMapLen)
    let isTree = treeMap[offset]
    switch isTree {
    | "#" => Some(1)
    | "." => None
    | _ => None
    }
  })
  ->Belt.Array.keepMap(v => v)
}

// Part One
let p1Slope = (3, 1)

let treesP1 = inputs->getTrees(p1Slope)->Belt.Array.reduce(0, (acc, v) => acc + v)->Js.log

// Part Two
let p2Slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

let treesP2 = p2Slopes
->Belt.Array.map(p2Slope => {
  inputs->getTrees(p2Slope)->Belt.Array.reduce(0, (acc, v) => acc + v)
})
->Belt.Array.reduce(1.0, (acc, t) => acc *. Belt.Int.toFloat(t))
->Js.log
