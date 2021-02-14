type input_t = array<string>

type color_t = string

type node_t = {
  color: string,
  count: int,
}

type bag_str_t = array<(string, string)>

type bag_t = {color: string, adjacents: array<node_t>}

type bags_t = array<bag_t>

module StrCmp = Belt.Id.MakeComparable({
  type t = string
  let cmp = Pervasives.compare
})

/* MapString
{
   "shiny gold": "1 dark olive , 2 vibrant plum",
   ...
}
*/
let parseSons = (sons: string) => {
  sons
  ->Js.String2.split(" , ")
  ->Belt.Array.map(content => {
    let qc = content->Js.String2.splitByRe(%re("/ (.*)/"))
    {
      color: qc[1]->Belt.Option.getExn,
      count: qc[0]->Belt.Option.getExn->int_of_string,
    }
  })
}

let parseReverse = (inputs: input_t): bag_str_t =>
  inputs
  ->Belt.Array.map(input => {
    let kv =
      input
      ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
      ->Js.String2.replace("no", "0")
      ->Js.String2.trim
      ->Js.String2.split("  contain ")

    let vertices = kv[1]->parseSons
    let color = kv[0]

    vertices->Belt.Array.map(vertex => {
      (vertex.color, color)
    })
  })
  ->Belt.Array.concatMany

let parse = (inputs: input_t): bags_t =>
  inputs->Belt.Array.map(input => {
    let kv =
      input
      ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
      ->Js.String2.replace("no", "0")
      ->Js.String2.trim
      ->Js.String2.split("  contain ")

    let adjacents = kv[1]->parseSons
    let color = kv[0]

    {color: color, adjacents: adjacents}
  })

let findTargetColorFromBags = (bags, targetColor) => {
  bags->Belt.Map.String.getExn(targetColor)
}

let toUnique = (arr: array<string>): array<string> => {
  arr->Belt.Set.fromArray(~id=module(StrCmp))->Belt.Set.toArray
}

let getColors = (contents: array<node_t>): array<string> => contents->Belt.Array.map(c => c.color)

let getColorCounts = (contents: array<node_t>): array<int> => contents->Belt.Array.map(c => c.count)

let getColor = (node: node_t): string => node.color

let isNotNoOtherFromMpde = (parsedAdjacents: array<node_t>) =>
  parsedAdjacents->Belt.Array.keep(node => node.color !== "other")

let isNotOtherFromColor = (colors: array<string>) => colors->Belt.Array.keep(c => c !== "other")

let findAdjacentNodes = (targetColors, bags) => {
  targetColors
  ->Belt.Array.map(targetColor => bags->findTargetColorFromBags(targetColor))
  ->Belt.Array.map(adjacent => adjacent->parseSons)
  ->Belt.Array.concatMany
}

// let rec search = (bags, targetColors: array<string>, vertices: array<node_t>) => {
//   let adjacentNodes = targetColors->isNotOtherFromColor->findAdjacentNodes(bags)
//   switch adjacentNodes->Belt.Array.length == 0 {
//   | true => vertices
//   | false => search(bags, adjacentNodes->getColors, vertices->Belt.Array.concat(adjacentNodes))
//   }
// }

let rec search = (bags: bags_t, targetColor: color_t) => {
  bags->Belt.Array.reduce([], (acc, item) => {
    Js.log(("acc, item", acc, item.color))

    switch item.color == targetColor {
    | true =>
      // item.adjacents->Belt.Array.map(adj => adj.color->search)
      // acc->Belt.Array.concat([item])
      item.adjacents->Belt.Array.map(adj => {
        bags->search(adj.color)
      })
    | false => acc
    }
  })
}

let getLength = arr => arr->Belt.Array.length

let inputs = Node.Fs.readFileAsUtf8Sync("./sample_p2.txt")->Js.String2.split("\n")

let targetColor = "shiny gold"

// let uniqueKeyColors = inputs->parse->Belt.Map.String.keysToArray->toUnique

// uniqueKeyColors
// ->Belt.Array.map(keyColor => {
//   inputs->parse->search([keyColor], [])->Belt.Array.keep(v => v.color != "other")
// })
// ->Belt.Array.keepMap(r => {
//   let thisArr = r->Belt.Array.keep(v => v.color != "other")
//   // Js.log(("thisArr:", thisArr, thisArr->Belt.Array.length))
//   // let tailColor = thisArr[(thisArr->Belt.Array.length) - 1]
//   // tailColor
//   // thisArr->Belt.List.fromArray->Belt.List.tail
// })
// ->Js.log

// Part 1 # DOING
// inputs
// ->parseReverse
// ->Js.log

// Part 2
inputs
->parse
// bags[0]
->search(targetColor)
->Js.log

// ->getColorCounts

/*
shiny gold 
    -> 1 dark olive
        -> 3 faded blue     -> 0 other
        -> 4 dotted black   -> 0 other
    -> 2 vibrant plum
        -> 5 faded blue      -> 0 other
        -> 6 dotted black    -> 0 other
*/
