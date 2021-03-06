type spreadsheet = array<array<int>>

type diffChecksum = int

type diffChecksums = array<diffChecksum>

type divChecksum = int

type divChecksums = array<divChecksum>

let spreadsheet: spreadsheet =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(row =>
    row->Js.String2.split("\t")->Belt.Array.keepMap(str => str->Belt.Int.fromString)
  )

let sortInt = (spreadsheet: spreadsheet) =>
  spreadsheet->Belt.Array.map(cols => cols->CmpUtil.sortIntArray)

let getDiffChecksum = (spreadsheet: spreadsheet): diffChecksums =>
  spreadsheet->Belt.Array.map(cols => {
    let (min, max) = (cols[0], cols[cols->Belt.Array.length - 1])
    max - min
  })

let getDivChecksum = (spreadsheet: spreadsheet): divChecksums => {
  spreadsheet
  ->Belt.Array.map(cols => {
    cols
    ->Belt.Array.map(colA => {
      cols->Belt.Array.keepMap(colB => {
        let remainder = mod(colA, colB)
        let quotient = colA / colB

        switch remainder === 0 && quotient !== 1 {
        | true => Some(quotient)
        | false => None
        }
      })
    })
    ->Belt.Array.concatMany
  })
  ->Belt.Array.concatMany
}

let sum = (checksums: array<int>) => checksums->Belt.Array.reduce(0, (acc, item) => acc + item)

// Part 1
spreadsheet->sortInt->getDiffChecksum->sum->Js.log

// Part 2
spreadsheet->sortInt->getDivChecksum->sum->Js.log
