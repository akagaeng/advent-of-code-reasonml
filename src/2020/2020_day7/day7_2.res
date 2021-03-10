type input_t = array<string>

type color_t = string

type node_t = {
  color: string,
  count: int,
}

type bag_str_t = array<(string, string)>

type bag_t = {color: string, contents: array<node_t>}

type bags_t = array<bag_t>

module StrCmp = Belt.Id.MakeComparable({
  type t = string
  let cmp = Pervasives.compare
})

let parseContents = (sons: string) => {
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

// part1
let parseEdges = (inputs: input_t): bag_str_t =>
  inputs
  ->Belt.Array.map(input => {
    let kv =
      input
      ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
      ->Js.String2.replace("no", "0")
      ->Js.String2.trim
      ->Js.String2.split("  contain ")

    let color = kv[0]
    let contents = kv[1]->parseContents

    contents->Belt.Array.map(content => {
      (color, content.color)
    })
  })
  ->Belt.Array.concatMany

// part2
let parse = (inputs: input_t): bags_t =>
  inputs->Belt.Array.map(input => {
    let kv =
      input
      ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
      ->Js.String2.replace("no", "0")
      ->Js.String2.trim
      ->Js.String2.split("  contain ")

    let contents = kv[1]->parseContents
    let color = kv[0]

    {color: color, contents: contents}
  })

let toUniqueArray = (arr: array<string>): array<string> => {
  arr->Belt.Set.fromArray(~id=module(StrCmp))->Belt.Set.toArray
}

let findBagWith = (bags: bags_t, targetColor) => {
  bags->Belt.Array.keepMap(bag => {
    switch bag.color == targetColor {
    | true => Some(bag)
    | false => None
    }
  })
}

// part 1
let rec searchEdge = (bagStrs: bag_str_t, targetColor: color_t) => {
  let foundColoredBags = bagStrs->Belt.Array.keepMap(((color, contentColor)) => {
    switch contentColor == targetColor {
    | true => Some(color)
    | false => None
    }
  })

  foundColoredBags->Belt.Array.reduce(foundColoredBags, (acc, foundColoredBag) => {
    acc->Belt.Array.concat(bagStrs->searchEdge(foundColoredBag))
  })
}

// part 2
let rec search = (bags: bags_t, targetColor: color_t) => {
  let foundBag = bags->findBagWith(targetColor)->Belt.Array.get(0)

  switch foundBag {
  | Some(foundBag) =>
    foundBag.contents->Belt.Array.reduce(0, (acc, content) => {
      acc + content.count + bags->search(content.color) * content.count
    })
  | None => 1
  }
}

let finalizeP1 = arr => {
  arr->toUniqueArray->Belt.Array.length
}

let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

let targetColor = "shiny gold"

// Part 1
inputs->parseEdges->searchEdge(targetColor)->finalizeP1->Js.log

// Part 2
inputs->parse->search(targetColor)->Js.log

/*
shiny gold
  -> 1 dark olive = 1 * (3 + 4)
    -> 3 faded blue     -> 0 other
    -> 4 dotted black   -> 0 other
  -> 2 vibrant plum = 2
    -> 5 faded blue      -> 0 other = 2 * 5
    -> 6 dotted black    -> 0 other = 2 * 6

sum = 1 + 1 * (3 + 4) + 2 + 2 * (5 + 6)

shiny gold 
  -> 2 dark red
    -> 2 dark orange
      -> 2 dark yellow
        -> 2 dark green
          -> 2 dark blue
            -> 2 dark violet
              -> no other

sum = 2 + 2 * (2 + 2 * (2 + 2 * (2 + 2 * (2 + 2 * (2 + 2 * 0))))
*/
