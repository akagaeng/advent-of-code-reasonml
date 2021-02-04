let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n\n")

// Part 1
inputs
->Belt.Array.map(group => {
  group
  ->Js.String2.split("\n")
  ->Belt.Array.map(q => q->Js.String2.split(""))
  ->Belt.Array.concatMany
  ->Belt.Set.String.fromArray
  ->Belt.Set.String.toArray
  ->Belt.Array.length
})
->Belt.Array.reduce(0, (acc, v) => acc + v)
->Js.log

// Part 2
let findUnion = arr =>
  arr->Belt.Array.concatMany->Belt.Set.String.fromArray->Belt.Set.String.toArray

let findIntersect = (arr1, arr2) =>
  Belt.Set.String.intersect(
    arr1->Belt.Set.String.fromArray,
    arr2->Belt.Set.String.fromArray,
  )->Belt.Set.String.toArray

inputs
->Belt.Array.map(group =>
  group->Js.String2.split("\n")->Belt.Array.map(g => g->Js.String2.split(""))
)
->Belt.Array.map(question =>
  Belt.Array.reduce(question, question->findUnion, findIntersect)->Belt.Array.length
)
->Belt.Array.reduce(0, (acc, v) => acc + v)
->Js.log
