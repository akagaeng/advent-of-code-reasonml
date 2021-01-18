// map with index

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))

let idxMax = Belt.Array.length(inputs) - 1

// Part One
let vi = Belt.Array.range(0, idxMax)->Belt.Array.map(i => {
  let js = Belt.Array.range(i + 1, idxMax)
  js->Belt.Array.map(j => {
    let x = inputs[i]
    let y = inputs[j]

    let sum = x + y
    if sum === 2020 {
      let mul = x * y
      Js.log("Part One: " ++ Belt.Int.toString(mul))
    }
  })
})

// Part Two
let vi = Belt.Array.range(0, idxMax)->Belt.Array.map(i => {
  let js = Belt.Array.range(i + 1, idxMax)
  js->Belt.Array.map(j => {
    let ks = Belt.Array.range(j + 1, idxMax)
    ks->Belt.Array.map(k => {
      let x = inputs[i]
      let y = inputs[j]
      let z = inputs[k]

      let sum = x + y + z
      if sum === 2020 {
        let mul = x * y * z
        Js.log("Part One: " ++ Belt.Int.toString(mul))
      }
    })
  })
})
