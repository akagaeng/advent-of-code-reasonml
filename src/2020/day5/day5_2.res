let inputs = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n")
let splitIdx = 7

let rowRange = (0, 127)
let colRange = (0, 7)

let lowerHalf = ((min: int, max: int)) => {
  let diff = (max - min + 1) / 2
  (min, max - diff)
}

let upperHalf = ((min: int, max: int)) => {
  let diff = (max - min + 1) / 2
  (min + diff, max)
}

// Q> | "F" || "L" 불가능?
let moveLoc = (character, (min: int, max: int)) => {
  switch character {
  | "F" => (min, max)->lowerHalf
  | "B" => (min, max)->upperHalf
  | "L" => (min, max)->lowerHalf
  | "R" => (min, max)->upperHalf
  | _ => (-1, -1)
  }
}

// characters -> colID / rowID
let search = (characters, sw) => {
  // Js.log(("characters", characters))
  characters->Belt.Array.map(character => {
    switch sw {
    | "row" => character->moveLoc(rowRange)
    | "col" => character->moveLoc(colRange)
    | _ => (-1, -1)
    }
  })
}

let splitSeatAt = (seat, splitAt) => {
  let codes = seat->Js.String2.split("")
  let row = codes->Belt.Array.keepWithIndex((_, i) => i < splitAt)
  let col = codes->Belt.Array.keepWithIndex((_, i) => i >= splitAt)
  (row, col)
}

// Part one
let outs = inputs->Belt.Array.map(input => {
  let (rows, cols) = input->splitSeatAt(splitIdx)
  rows->search("row")
  // rows->Belt.Array.map(r => r)
  // (initialRowRange, rows)->search->Js.log
  // (initialColRange, cols)->search->Js.log
})

Js.log(outs)

// Part two
