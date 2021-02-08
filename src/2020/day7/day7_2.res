/* Try instead
type node('node_t, 'extra) = ('node_t, array(('extra, 'node_t)))
type graph = array(node);
*/

type input_t = array<string>

// Node
// value = color
// extra = quantity
type contents_t = {
  value: string,
  extra: int,
}

type node_t = {
  value: string,
  // extra: int,
  adjacents: array<contents_t>,
}

type graph_t = array<node_t>

let parse = (inputs: input_t) => {
  // : graph_t

  inputs->Belt.Array.map(sen => {
    let kv =
      sen
      ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
      ->Js.String2.replace("no", "0")
      ->Js.String2.trim
      ->Js.String2.split("  contain ")

    let color = kv[0]
    let nodesArr =
      kv[1]
      ->Js.String2.split(" , ")
      ->Belt.Array.map(content => {
        let qc = content->Js.String2.splitByRe(%re("/ (.*)/"))

        // color, quantity, value
        (color, qc[1]->Belt.Option.getExn, qc[0]->Belt.Option.getExn->int_of_string)
      })

    nodesArr
    // let adjacents = Belt.Array.reduce([], (acc, item) => acc->Belt.Array.concat([{value, extra}]))
    // {value: color, nodes)

    // type node_t = {
    //   value: string,
    //   extra: int,
    //   adjacents: array<contents_t>,
    // }

    // {
    //   value: nodesArr[0]->Belt.Int.toString,
    //   extra: nodesArr[2],
    //   adjacents: [nodesArr[1]],
    // }

    // nodes
  })
  // ->Belt.Array.concatMany
}

// let parseBag = rawBags =>
//   rawBags
//   ->Belt.Array.map(rawBag => {
//     rawBag.contents
//     ->Js.String2.split(" , ")
//     ->Belt.Array.map(quantityAndColor => {
//       let cntColor = quantityAndColor->Js.String2.splitByRe(%re("/ (.*)/"))
//       let contents = {
//         quantity: cntColor[0]->Belt.Option.getExn->int_of_string,
//         color: cntColor[1]->Belt.Option.getExn,
//       }
//       {color: rawBag.color, contents: contents}
//     })
//   })
//   ->Belt.Array.concatMany

// let findColorExists = (target, value) => {
//   target->Belt.List.has(value, (tar, val) => tar.color === val.color)
// }

let sumCnt = arr => arr->Belt.Array.reduce(0, (a, b) => a + b)

let uniqueArray = arr => arr->Belt.Set.String.fromArray->Belt.Set.String.toArray

// let findBagDirectlyContainingColor = (bags: array<bag_t>, colorToFind: string) =>
//   bags->Belt.Array.keepMap(bag =>
//     switch bag.contents.color == colorToFind {
//     | true => Some(bag.color)
//     | false => None
//     }
//   )

let contains = (union: array<string>, subset: array<string>) => {
  let unionList = union->Belt.List.fromArray
  let subsetList = subset->Belt.List.fromArray

  subsetList->Belt.List.every(v => unionList->Belt.List.has(v, (a, b) => a == b))
}

// let rec findBagInDirectlyContainingColor = (colors, bags) => {
//   let parentColors = colors
//   ->Belt.Array.map(indirectColor => {
//     bags->findBagDirectlyContainingColor(indirectColor)
//   })
//   ->Belt.Array.concatMany
//   ->uniqueArray

//   switch colors->contains(parentColors) {
//   | true => colors
//   | false => findBagInDirectlyContainingColor(colors->Belt.Array.concat(parentColors), bags)
//   }
// }

let bags = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n")
// ->parse
// ->parseBag

let targetColor = "shiny gold"

let removeTargetColor = (arr, targetColor) => arr->Belt.Array.keep(v => v !== targetColor)

// Part 1
bags->parse->Js.log
// ->findBagDirectlyContainingColor(targetColor)
// ->findBagInDirectlyContainingColor(bags)
// ->uniqueArray
// ->removeTargetColor(targetColor)
// ->Belt.Array.length

// Part 2

// p1: find parents
// p2: find sons
