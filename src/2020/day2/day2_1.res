let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(str => {
    let [nums, letterd, password] = Js.String2.split(str, " ")
    let [numsMin, numsMax] = Js.String2.split(nums, "-")
    let [letter, _] = Js.String2.split(letterd, ":")
    let min = numsMin->Belt.Int.fromString->Belt.Option.getExn
    let max = numsMax->Belt.Int.fromString->Belt.Option.getExn
    (min, max, letter, password)
  })

let countLettersFromPassword = (letter, password) => {
  Js.String2.split(password, "")->Belt.Array.keep(v => v === letter)->Belt.Array.length
}

let validateRange = (count, min, max) => count >= min && count <= max

let p1 =
  inputs
  ->Belt.Array.keep(((min, max, letter, password)) => {
    let cnt = countLettersFromPassword(letter, password)
    validateRange(cnt, min, max)
  })
  ->Belt.Array.length

Js.log(p1)

let xor = (a, b) => {
  if (a === true && b === false) || (a === false && b === true) {
    true
  } else {
    false
  }
}

let validatePosition = (firstLoc, secondLoc, letter, password) => {
  let passwords = Js.String2.split(password, "")
  let isLocatedInFirst = passwords[firstLoc - 1] === letter
  let isLocatedInSecond = passwords[secondLoc - 1] === letter
  xor(isLocatedInFirst, isLocatedInSecond)
}

let p2 =
  inputs
  ->Belt.Array.keep(((firstLoc, secondLoc, letter, password)) => {
    validatePosition(firstLoc, secondLoc, letter, password)
  })
  ->Belt.Array.length

Js.log(p2)
