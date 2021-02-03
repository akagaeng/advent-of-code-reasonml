let inputs = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n\n")

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
inputs
->Belt.Array.map(group => {
  group->Js.String2.split("\n")->Belt.Array.map(q => q->Js.String2.split(""))
})
->Js.log
