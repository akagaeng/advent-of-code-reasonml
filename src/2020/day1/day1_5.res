// keep, map, tuple

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))

let outX = Belt.Array.map(inputs, x => {
  let outY = Belt.Array.map(inputs, y => {
    (x, y)
  })->Belt.Array.keep(((x, y)) => x + y === 2020)
  outY
})->Belt.Array.concatMany

let (resX, resY) = outX[0]
let res1 = resX * resY
Js.log(res1)
