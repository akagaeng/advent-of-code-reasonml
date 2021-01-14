# Soultion

## Versions

* [Initial version](#initial-version)
  * Using for loop
* Refactor
  * [Input with pipe](#input-with-pipe)
## Initial version

```reasonml
let input = Node.Fs.readFileAsUtf8Sync("src/2020/day1/input.txt")
let inputsStr = Js.String.split("\n", input)
let inputsOptionInt = Belt.Array.map(inputsStr, v => Belt.Int.fromString(v))
let inputsInt = Belt.Array.map(inputsOptionInt, v => Belt.Option.getExn(v))

let idxMax = Belt.Array.length(inputsInt) - 1

// Part One
for i in 0 to idxMax {
  for j in i + 1 to idxMax - i {
    let x = inputsInt[i]
    let y = inputsInt[j]
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
      let x = inputsInt[i]
      let y = inputsInt[j]
      let z = inputsInt[k]
      let sum = x + y + z

      if sum === 2020 {
        let mul = x * y * z
        Js.log("Part Two: " ++ Belt.Int.toString(mul))
      }
    }
  }
}

```

## Input with pipe

```reasonml
let inputStr = Node.Fs.readFileAsUtf8Sync("src/2020/day1/input.txt")
let inputs = Js.String.split("\n", inputStr)
                        -> Belt.Array.map(i => Belt.Int.fromString(i))
                        -> Belt.Array.map(o => Belt.Option.getExn(o))
```