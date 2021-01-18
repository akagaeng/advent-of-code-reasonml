// map, keep

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))

// Part One
let out =
  Belt.Array.map(inputs, x => {
    Belt.Array.map(inputs, y => {
      if x + y === 2020 {
        let mul = x * y
        Some(mul)
      } else {
        None
      }
    })
  })
  ->Belt.Array.map(a =>
    a->Belt.Array.keep(v => {
      switch v {
      | Some(_) => true
      | None => false
      }
    })
  )
  ->Belt.Array.concatMany

let res1 = out[0]
Js.log(res1)
