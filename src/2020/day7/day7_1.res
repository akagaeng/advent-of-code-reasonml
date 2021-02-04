let inputs =
  Node.Fs.readFileAsUtf8Sync("./sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(sen => {
    let kv =
      sen
      ->Js.String2.replaceByRe(%re("/bags|bag|[.]/g"), "")
      ->Js.String2.replace("no", "0")
      ->Js.String2.trim
      ->Js.String2.split("  contain ")
    (kv[0], kv[1])
  })
  ->Belt.Map.String.fromArray
  ->Belt.Map.String.map(v =>
    v
    ->Js.String2.split(" , ")
    ->Belt.Array.map(vvv => {
      let cntColor = vvv->Js.String2.splitByRe(%re("/ (.*)/"))
      let (cnt, color) = (cntColor[0], cntColor[1])
      (cnt, color)
    })
  )
  ->Belt.Map.String.toArray
  ->Belt.Map.String.fromArray

// Part 1
inputs->Js.log

// Part 2
inputs->Js.log
