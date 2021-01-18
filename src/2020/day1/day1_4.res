// keepMap

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))

// Part One
let out = Belt.Array.map(inputs, x => {
  Belt.Array.keepMap(inputs, y => {
    switch x + y {
    | 2020 => Some(x * y)
    | _ => None
    }
  })
})->Belt.Array.concatMany

let res1 = out[0]
Js.log(res1)
