// Adjacency matrix

type mat_t = {
  i: string,
  j: string,
}

let bags = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n")

module StrCmp = Belt.Id.MakeComparable({
  type t = string
  let cmp = Pervasives.compare
})

let sortArray = arr => {
  Belt.Set.fromArray(arr, ~id=module(StrCmp))->Belt.Set.toArray
}

let parse = inputs =>
  inputs
  ->Belt.Array.map(input => {
    let kv =
      input
      ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
      ->Js.String2.replace("no", "0")
      ->Js.String2.trim
      ->Js.String2.split("  contain ")

    let colorKey = kv[0]
    // let qcs =
    kv[1]
    ->Js.String2.split(" , ")
    ->Belt.Array.map(qc => {
      let qcout = qc->Js.String2.splitByRe(%re("/ (.*)/"))
      (colorKey, qcout[1], qcout[0])
    })
  })
  ->Belt.Array.concatMany

let toMatrix = tuples => {
  let dim = tuples->Belt.Array.map(((c, _, _)) => c)->sortArray
  let dimLen = dim->Belt.Array.length

  let out = Belt.Array.make(dimLen, 0)
//   dimLen
// out
}

// Part 1
bags
->parse
->toMatrix
// ->search([targetColor])
// ->sum
->Js.log
