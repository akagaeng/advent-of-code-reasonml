let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")
let codeLen = 10
let rowCodeLen = 7
let colCodeLen = 3
let colLen = 8 // 2^3
let binSeatsColSet = ["000", "001", "010", "011", "100", "101", "110", "111"]

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

let binPow = (exp: int) => Js.Math.pow_float(~base=2.0, ~exp=exp->Belt.Int.toFloat)->int_of_float

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
    if i < colCodeLen {
      acc + digit * binPow(i)
    } else {
      acc + 8 * digit * binPow(i - colCodeLen)
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

// 0 ~ 127 -> 1~126 (not front or back)
// let rowRange = Belt.Array.range(1, binPow(rowCodeLen) - 1 - 1)->Js.log

let binSeatsRowSet =
  binSeats
  ->sortStringArray
  ->Belt.Array.map(sortedSeat => sortedSeat->Js.String2.slice(~from=0, ~to_=rowCodeLen))
  ->Belt.Set.fromArray(~id=module(StrCmp))
  ->Belt.Set.toArray
// ->Belt.Array.length
// ->Js.log

let binSeatsArray = binSeats
->sortStringArray
->Belt.Array.mapWithIndex((i, sortedSeat) => {
  let row = sortedSeat->Js.String2.slice(~from=0, ~to_=rowCodeLen)
  let col = sortedSeat->Js.String2.slice(~from=rowCodeLen, ~to_=codeLen)

  let idx = mod(i, colLen)
  let nextIdx = idx + 1

  // switch idx < 7 {
  //   Some()
  // }
  (row, col, sortedSeat, i, idx, nextIdx, binSeatsRowSet[nextIdx])
  // colLen
})
->Js.log
// ->Belt.Array.length

// binSeatsRowSet->Belt.Array.length->Js.log // 118
binSeatsColSet->Js.log
// ->Belt.Array.length // 8

// binSeatsArray->Js.Dict.fromArray->Js.Dict.keys->Js.log

// binSeatsRowSet
// ->Belt.Array.map(s => )
