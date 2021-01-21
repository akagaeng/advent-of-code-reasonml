// array of array

let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n\n")
let fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

// Part One
let passportRequiredFields = fields->Belt.Array.keep(f => f !== "cid")

let isPresent = (dict, keys) => {
  let keyFields = Belt.Array.length(keys)
  let validFields = keys
  ->Belt.Array.map(key => {
    let value = dict->Belt.Map.String.get(key)
    switch value {
    | Some(value) => Some(value)
    | _ => None
    }
  })
  ->Belt.Array.keepMap(o => o)
  ->Belt.Array.length

  switch keyFields - validFields {
  | 0 => 1
  | _ => 0
  }
}

let passports =
  inputs->Belt.Array.map(v =>
    v
    ->Js.String2.split("\n")
    ->Belt.Array.map(f => f->Js.String2.split(" "))
    ->Belt.Array.concatMany
    ->Belt.Array.map(s => s->Js.String2.split(":"))
    ->Belt.Array.map(arr => (arr[0], arr[1]))
    ->Belt.Map.String.fromArray
  )

passports
->Belt.Array.map(v => isPresent(v, passportRequiredFields))
->Belt.Array.reduce(0, (a, b) => a + b)
->Js.log

// Part two
