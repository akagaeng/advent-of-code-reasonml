type input_t = array<string>

type contents_t = {
  value: string,
  extra: int,
}

type node_t = {
  value: string,
  adjacents: string, // -> array<contents_t>
}

type graph_t = array<node_t>

// Return: MapString type def?
let parse = (inputs: input_t) =>
  inputs
  ->Belt.Array.map(input => {
    let kv =
      input
      ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
      ->Js.String2.replace("no", "0")
      ->Js.String2.trim
      ->Js.String2.split("  contain ")
    (kv[0], kv[1])
  })
  ->Belt.Map.String.fromArray

/* MapString
{
   "shiny gold": "1 dark olive , 2 vibrant plum",
   ...
}
*/
let parseAdjacents = (adjacents:string) => {
  adjacents
  ->Js.String2.split(" , ")
  ->Belt.Array.map(content => {
    let qc = content->Js.String2.splitByRe(%re("/ (.*)/"))
    {
      value: qc[1]->Belt.Option.getExn,
      extra: qc[0]->Belt.Option.getExn->int_of_string,
    }
  })
}

// let search = (bags, targetColor) => {
//   let adjacents = bags->Belt.Map.String.get(targetColor)

//   adjacents->parseAdjacents
//   // adjacents
// }
// {
//   // let parentColorAndContentArr =
//     inputs
//     ->Belt.Array.map(input => {
//       // let kv =
//         input
//         ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
//         ->Js.String2.replace("no", "0")
//         ->Js.String2.trim
//         ->Js.String2.split("  contain ")

//       // let color = kv[0]
//       // let parentColorAndContent =
//       //   kv[1]
//       //   ->Js.String2.split(" , ")
//       //   ->Belt.Array.map(content => {
//       //     let qc = content->Js.String2.splitByRe(%re("/ (.*)/"))
//       //     (
//       //       color,
//       //       {
//       //         value: qc[1]->Belt.Option.getExn,
//       //         extra: qc[0]->Belt.Option.getExn->int_of_string,
//       //       },
//       //     )
//       //   })
//       // parentColorAndContent
//     })
//     ->Belt.Array.concatMany

//   let graph = parentColorAndContentArr->Belt.Array.reduce([], (
//     acc,
//     (color: string, node: contents_t),
//   ) => {
//     let contains = (graph: graph_t, color: string): bool => {
//       graph->Belt.Array.some(g => {
//         g.value == color
//       })
//     }

//     // switch acc->contains(color) {
//     // | true => {
//     //     let lastElement = [acc->Belt.Array.getExn(acc->Belt.Array.length - 1)]
//     //     // https://rescript-lang.org/docs/manual/latest/api/belt/array#set

//     //     // acc->Belt.Array.getExn(acc->Belt.Array.length - 1)
//     //     // adjacents
//     //     let adjacents = Belt.Array.get();
//     //     acc->Belt.Array.set(index, {
//     //       {
//     //          value: color,
//     //          adjacents: old_adjacents->Belt.List.add(newNode)
//     //        }
//     //     })
//     //     acc

//     //     // Js.log(("acc", acc, lastElement))
//     //     // lastElement
//     //     // acc->Belt.Array.concat([
//     //     //   {
//     //     //     value: color,
//     //     //     adjacents: [node, ...],
//     //     //   },
//     //     // ])
//     //     acc->Belt.Array.
//     //   }
//     // // [acc->Belt.Array.getExn(acc->Belt.Array.length - 1)]->Belt.Array.concat([node])
//     // | false =>
//     //   // 없는 경우, k, v 다 넣음
//     //   acc->Belt.Array.concat([
//     //     {
//     //       value: color,
//     //       adjacents: [node],
//     //     },
//     //   ])
//     // }
//   })

//   graph
// }->Belt.Array.map(v => (v.value, v.adjacents->Belt.Array.map(v => v.value)))

let bags = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n")

let targetColor = "shiny gold"

// Part 1
bags
->parse
// ->search(targetColor)
// ->sum
->Js.log

// Part 2

// p1: find parents
// p2: find sons
