// keep, map, tuple

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))

let outX = Belt.Array.map(inputs, x => {
  let outY =
    Belt.Array.map(inputs, y => {
      (x + y, x * y)
    })
    ->Belt.Array.keep(((sum, _)) => sum === 2020)
    ->Belt.Array.map(((_, mul)) => mul)
  outY
})->Belt.Array.concatMany

let res1 = outX[0]
Js.log(res1)
