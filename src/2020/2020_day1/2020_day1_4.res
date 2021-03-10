// keepMap

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))

// Part One
let p1 = Belt.Array.map(inputs, x => {
  Belt.Array.keepMap(inputs, y => {
    switch x + y {
    | 2020 => Some(x * y)
    | _ => None
    }
  })
})->Belt.Array.concatMany

Js.log(p1[0])

// Part Two
let p2 = Belt.Array.map(inputs, x => {
  Belt.Array.map(inputs, y => {
    Belt.Array.keepMap(inputs, z => {
      switch x + y + z {
      | 2020 => Some(x * y * z)
      | _ => None
      }
    })
  })->Belt.Array.concatMany
})->Belt.Array.concatMany

Js.log(p2[0])
