// keep, map, tuple

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))

// Part One
let p1 = Belt.Array.map(inputs, x => {
  Belt.Array.map(inputs, y => {
    (x, y)
  })->Belt.Array.keep(((x, y)) => x + y === 2020)
})->Belt.Array.concatMany

let (p1X, p1Y) = p1[0]
Js.log(p1X * p1Y)

// Part Two
let p2 = Belt.Array.map(inputs, x => {
  Belt.Array.map(inputs, y => {
    Belt.Array.map(inputs, z => {
      (x, y, z)
    })->Belt.Array.keep(((x, y, z)) => x + y + z === 2020)
  })->Belt.Array.concatMany
})->Belt.Array.concatMany

let (p2X, p2Y, p2Z) = p2[0]
Js.log(p2X * p2Y * p2Z)
