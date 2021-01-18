// recursion, switch

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))

