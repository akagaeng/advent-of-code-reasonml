let inputs = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n")
let splitIdx = 7

let initialRowRange = (0, 127)
let initialColRange = (0, 7)

// 자리를 까먹음
// 인접 항공권 조회 -> puzzle input
// process of elimination 을 통해 자리를 찾을 수 있음

// Seats구성
// - (x) zones of groups
// - (o) binary space partitioning

// seat: BFFFBBFRRR
// 앞 7자리: F or B (row 행: 128개 0~127)
//   - front
//   - back
// 뒤 3자리: L or R (col 열: 64개 0~63)
//   - left
//   - right

// (x + y + 1) / 2 - 1
// LLL -> (0, 7) -> (0, 3) -> (0, 1) -> (0, 0)
// min == max 인 경우 found

let rec search = ((min, max), lowerOrUpper) => {
  switch lowerOrUpper {
    | "L" => (min, max)
    | "R" => (min, max)
  }
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
  // rows->Belt.Array.map(r => r)
  // (initialRowRange, rows)->search->Js.log
  (initialColRange, cols)->search->Js.log
})

// Js.log(outs[0])

// Part two
