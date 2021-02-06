type raw_bag_t = {
  raw_color: string,
  raw_contents: string,
}

type contents_t = {
  contents_quantity: int,
  contents_color: string,
}

type bag_t = {
  color: string,
  contents: contents_t,
}

let parseRawBag = sentences => {
  sentences->Belt.Array.map(sen => {
    let kv =
      sen
      ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
      ->Js.String2.replace("no", "0")
      ->Js.String2.trim
      ->Js.String2.split("  contain ")
    {raw_color: kv[0], raw_contents: kv[1]} // raw_bag_t
  })
}

let parseBag = rawBags =>
  rawBags
  ->Belt.Array.map(rawBag => {
    rawBag.raw_contents
    ->Js.String2.split(" , ")
    ->Belt.Array.map(quantityAndColor => {
      let cntColor = quantityAndColor->Js.String2.splitByRe(%re("/ (.*)/"))
      let contents = {
        contents_quantity: cntColor[0]->Belt.Option.getExn->int_of_string,
        contents_color: cntColor[1]->Belt.Option.getExn,
      }
      {color: rawBag.raw_color, contents: contents}
    })
  })
  ->Belt.Array.concatMany

let findColorExists = (target, value) => {
  target->Belt.List.has(value, (tar, val) => tar.color === val.color)
}

let sumCnt = arr => arr->Belt.Array.reduce(0, (a, b) => a + b)

let uniqueArray = arr => arr->Belt.Set.String.fromArray->Belt.Set.String.toArray

let findBagDirectlyContainingColor = (bags: array<bag_t>, colorToFind: string) =>
  bags->Belt.Array.keepMap(bag =>
    switch bag.contents.contents_color == colorToFind {
    | true => Some(bag.color)
    | false => None
    }
  )

let contains = (union: array<string>, subset: array<string>) => {
  let unionList = union->Belt.List.fromArray
  let subsetList = subset->Belt.List.fromArray

  subsetList->Belt.List.every(v => unionList->Belt.List.has(v, (a, b) => a == b))
}

let rec findBagInDirectlyContainingColor = (colors, bags) => {
  let parentColors = colors
  ->Belt.Array.map(indirectColor => {
    bags->findBagDirectlyContainingColor(indirectColor)
  })
  ->Belt.Array.concatMany
  ->uniqueArray

  switch colors->contains(parentColors) {
  | true => colors
  | false => findBagInDirectlyContainingColor(colors->Belt.Array.concat(parentColors), bags)
  }
}

let bags = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n")->parseRawBag->parseBag

let targetColor = "shiny gold"

let removeTargetColor = (arr, targetColor) => arr->Belt.Array.keep(v => v !== targetColor)

// Part 1
bags
->findBagDirectlyContainingColor(targetColor)
->findBagInDirectlyContainingColor(bags)
->uniqueArray
->removeTargetColor(targetColor)
->Belt.Array.length
->Js.log

// Part 2
let findBagDirectlyContainedByColor = (bags: array<bag_t>, colorToFind: string) =>
  bags->Belt.Array.keepMap(bag =>
    switch bag.color == colorToFind {
    // bag.color, bag.contents.contents_color 위치 바뀜
    | true =>
      Some({
        contents_color: bag.contents.contents_color,
        contents_quantity: bag.contents.contents_quantity,
      })
    | false => None
    }
  )

let filterContentsColor = (arr: array<contents_t>) => arr->Belt.Array.map(u => u.contents_color)

let contains2 = (union: array<contents_t>, subset: array<contents_t>) => {
  let unionList = union->filterContentsColor->Belt.List.fromArray
  let subsetList = subset->filterContentsColor->Belt.List.fromArray

  subsetList->Belt.List.every(v => unionList->Belt.List.has(v, (a, b) => a == b))
}

let uniqueArray2 = (arr: array<contents_t>) => {
  arr
  ->filterContentsColor
  ->Belt.Set.String.fromArray
  ->Belt.Set.String.toArray
  ->Belt.Array.getBy(color => {
    arr->Belt.Array.some(a => a.contents_color == color)
  })
}

let rec findBagInDirectlyContainedByColor = (
  colorsAndQuantity: array<contents_t>,
  bags,
  subSum,
) => {
  let parentColorsAndQuanty = colorsAndQuantity
  ->Belt.Array.map(indirectColorAndQuantity => {
    let {contents_quantity: parentQuantity, contents_color: parentColors} = indirectColorAndQuantity
    bags->findBagDirectlyContainedByColor(indirectColorAndQuantity.contents_color)
  })
  ->Belt.Array.concatMany
  ->uniqueArray2

  switch colorsAndQuantity->contains2(parentColorsAndQuanty) {
  | true => colorsAndQuantity
  | false =>
    findBagInDirectlyContainedByColor(
      colors->Belt.Array.concat(parentColorsAndQuanty),
      bags,
      subSum,
    )
  }
}

let rec findAndCountBagInDirectlyContainedByColor = (colors, bags, subSum) => {
  let parentColors = colors
  ->Belt.Array.map(indirectColor => {
    bags->findBagDirectlyContainedByColor(indirectColor)
  })
  ->Belt.Array.concatMany
  ->uniqueArray

  Js.log(("parentColors>", parentColors, "subSum:", "son quantity:"))

  switch colors->contains(parentColors) {
  | true => colors
  | false =>
    findBagInDirectlyContainedByColor(colors->Belt.Array.concat(parentColors), bags, subSum)
  }
}

bags
->findBagDirectlyContainedByColor(targetColor) // [ 'dark red' ]
->findAndCountBagInDirectlyContainedByColor(bags, 0)
// ->uniqueArray
// ->removeTargetColor(targetColor)
// ->Belt.Array.length
->Js.log

// p1: find parents
// p2: find sons
