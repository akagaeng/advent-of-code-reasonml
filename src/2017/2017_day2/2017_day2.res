type spreadsheet = array<array<int>>

type checksum = int

type checksums = array<checksum>

let spreadsheet: spreadsheet =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(row =>
    row->Js.String2.split("\t")->Belt.Array.keepMap(str => str->Belt.Int.fromString)
  )

let getDiffChecksum = (spreadsheet: spreadsheet): checksums =>
  spreadsheet->Belt.Array.map(cols => {
    let sortedRow = cols->CmpUtil.sortIntArray
    let (min, max) = (sortedRow[0], sortedRow[cols->Belt.Array.length - 1])
    max - min
  })

let sum = (checksums: checksums) => checksums->Belt.Array.reduce(0, (acc, item) => acc + item)

spreadsheet->getDiffChecksum->sum->Js.log
