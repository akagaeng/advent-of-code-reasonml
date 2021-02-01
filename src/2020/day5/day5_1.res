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

let sortBinSeats = arr => {
  Belt.Set.fromArray(arr, ~id=module(StrCmp))->Belt.Set.toArray
}

let binPow = (exp: int) => Js.Math.pow_float(~base=2.0, ~exp=exp->Belt.Int.toFloat)->int_of_float

let replaceCharsTo = (str: string, chars: string, toChar: string) => {
  switch chars {
  | "FL" => str->Js.String2.replaceByRe(%re("/[FL]/g"), toChar)
  | "BR" => str->Js.String2.replaceByRe(%re("/[BR]/g"), toChar)
  | _ => ""
  }
}

let binSeats =
  inputs->Belt.Array.map(input => input->replaceCharsTo("FL", "0")->replaceCharsTo("BR", "1"))

let calcCode = (str: string) =>
  str
  ->Js.String2.split("")
  ->Belt.Array.reverse
  ->Belt.Array.reduceWithIndex(0, (acc, v, i) => {
    let digit = v->int_of_string
    if i < colCodeLen {
      acc + digit * binPow(i)
    } else {
      acc + 8 * digit * binPow(i - colCodeLen)
    }
  })

let getLastOne = seats => seats[seats->Belt.Array.length - 1]

let getColDiff = ((arr1, arr2)) => {
  let arr1Set = Belt.Set.String.fromArray(arr1)
  let arr2Set = Belt.Set.String.fromArray(arr2)
  Belt.Set.String.toArray(Belt.Set.String.diff(arr1Set, arr2Set))
}

let binSeatToRowAndCol = binSeat => {
  let row = binSeat->Js.String2.slice(~from=0, ~to_=rowCodeLen)
  let col = binSeat->Js.String2.slice(~from=rowCodeLen, ~to_=codeLen)
  (row, col)
}

let arrayToJson = arr => {
  arr->Belt.Array.reduce(Js.Dict.empty(), (acc, (key, val)) => {
    // row: key, col: val
    let keyExists = Js.Dict.get(acc, key)

    switch keyExists {
    | Some(arr) => {
        Js.Dict.set(acc, key, Belt.Array.concat(arr, [val]))
        acc
      }
    | _ => {
        Js.Dict.set(acc, key, Belt.Array.concat([], [val]))
        acc
      }
    }
  })
}

let findOnly1MissingSeat = dict =>
  dict
  ->Js.Dict.entries
  ->Belt.Array.keepMap(((k, v)) => {
    let vLen = Belt.Array.length(v)

    switch vLen === 7 {
    | true => Some((k, getColDiff((binSeatsColSet, v))[0]))
    | false => None
    }
  })
  // concat k, v [ [ '1011100', '101' ] ] => "1011100101"
  ->Belt.Array.reduce("", (acc, (k, v)) => acc ++ k ++ v)

// Part one
binSeats->sortBinSeats->getLastOne->calcCode->Js.log

// Part two
binSeats->Belt.Array.map(binSeatToRowAndCol)->arrayToJson->findOnly1MissingSeat->calcCode->Js.log
