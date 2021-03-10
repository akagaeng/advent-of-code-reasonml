let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(str => {
    let strArr = str->Js.String2.split(" ")
    let (nums, letterd, password) = (strArr[0], strArr[1], strArr[2])
    let numsArr = Js.String2.split(nums, "-")
    let (numsMin, numsMax) = (numsArr[0], numsArr[1])
    let letter = Js.String2.split(letterd, ":")[0]
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
  switch (a, b) {
  | (true, false)
  | (false, true) => true
  | _ => false
  }
}

let validatePosition = ((firstLoc, secondLoc, letter, password)) => {
  let passwords = Js.String2.split(password, "")
  let isLocatedInFirst = passwords[firstLoc - 1] === letter
  let isLocatedInSecond = passwords[secondLoc - 1] === letter
  xor(isLocatedInFirst, isLocatedInSecond)
}

let p2 = inputs->Belt.Array.keep(validatePosition)->Belt.Array.length

Js.log(p2)
