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

let parseBag = rawBags => {
  rawBags->Belt.Array.map(rawBag => {
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
}

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")->parseRawBag->parseBag

// Part 1
inputs->Js.log

// Part 2
// inputs->Js.log
