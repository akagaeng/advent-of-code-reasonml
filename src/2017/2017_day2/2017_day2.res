type spreadsheet = array<array<int>>

type checksum = int

type checksums = array<checksum>

type diffChecksum = int

type diffChecksums = array<diffChecksum>

let spreadsheet: spreadsheet =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(row =>
    row->Js.String2.split("\t")->Belt.Array.keepMap(str => str->Belt.Int.fromString)
  )

let sortAsc = (spreadsheet: spreadsheet): spreadsheet =>
  spreadsheet->Belt.Array.map(cols => cols->CmpUtil.sortIntArray)

let sortedSpreadsheet = spreadsheet->sortAsc

let getDiffChecksum = (sortedSpreadsheet: spreadsheet): diffChecksums =>
  sortedSpreadsheet->Belt.Array.map(cols => {
    let (min, max) = (cols[0], cols[cols->Belt.Array.length - 1])
    max - min
  })

let sum = (checksums: checksums) => checksums->Belt.Array.reduce(0, (acc, item) => acc + item)

sortedSpreadsheet->getDiffChecksum->sum->Js.log
