let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")
let rowCharLen = 7
let colCharLen = 3

module StrCmp = Belt.Id.MakeComparable({
  type t = string
  let cmp = Pervasives.compare
})

let splitSeatAt = (seat, splitAt) => {
  let codes = seat->Js.String2.split("")
  let row = codes->Belt.Array.keepWithIndex((_, i) => i < splitAt)
  let col = codes->Belt.Array.keepWithIndex((_, i) => i >= splitAt)
  (row, col)
}

let sortStringArray = arr => {
  Belt.Set.fromArray(arr, ~id=module(StrCmp))->Belt.Set.toArray
}

let binPow = (exp: int) => Js.Math.pow_int(~base=2, ~exp)

// Part one
let binSeats =
  inputs->Belt.Array.map(input =>
    input->Js.String2.replaceByRe(%re("/[FL]/g"), "0")->Js.String2.replaceByRe(%re("/[BR]/g"), "1")
  )

let calcP1 = arr =>
  arr
  ->Belt.Array.reverse
  ->Belt.Array.reduceWithIndex(0, (acc, v, i) => {
    let digit = v->int_of_string
    if i < colCharLen {
      acc + digit * binPow(i)
    } else {
      acc + 8 * digit * binPow(i - colCharLen)
    }
  })

binSeats
->sortStringArray
->Belt.Array.keepWithIndex((_, i) => i === binSeats->Belt.Array.length - 1)
->Js.Array2.toString
->Js.String2.split("")
->calcP1
->Js.log

// Part two
