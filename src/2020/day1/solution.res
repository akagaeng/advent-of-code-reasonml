let inputStr = Node.Fs.readFileAsUtf8Sync("src/2020/day1/input.txt")
let inputs = Js.String.split("\n", inputStr)
                        -> Belt.Array.map(i => Belt.Int.fromString(i))
                        -> Belt.Array.map(o => Belt.Option.getExn(o))

let idxMax = Belt.Array.length(inputs) - 1

// Part One
for i in 0 to idxMax {
  for j in i + 1 to idxMax - i {
    let x = inputs[i]
    let y = inputs[j]
    let sum = x + y

    if sum === 2020 {
      let mul = x * y
      Js.log("Part One: " ++ Belt.Int.toString(mul))
    }
  }
}

// Part Two
for i in 0 to idxMax {
  for j in i + 1 to idxMax - i {
    for k in j + 1 to idxMax - j {
      let x = inputs[i]
      let y = inputs[j]
      let z = inputs[k]
      let sum = x + y + z

      if sum === 2020 {
        let mul = x * y * z
        Js.log("Part Two: " ++ Belt.Int.toString(mul))
      }
    }
  }
}
