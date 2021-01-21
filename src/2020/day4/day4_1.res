// array of array

let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n\n")
let fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

let passportRequiredFields = fields->Belt.Array.keep(f => f !== "cid")

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

let checkKeyExists = ((keys, dict)) => {
  keys->Belt.Array.map(key => {
    let value = dict->Belt.Map.String.get(key)
    switch value {
    | Some(value) => Some(value)
    | _ => None
    }
  })
}

let isPresent = ((dict, keys)) => {
  let keyFieldsLen = Belt.Array.length(keys)
  let validFieldsLen = (keys, dict)->checkKeyExists->Belt.Array.keepMap(o => o)->Belt.Array.length

  switch keyFieldsLen - validFieldsLen {
  | 0 => 1
  | _ => 0
  }
}

// Part One
passports
->Belt.Array.map(v => (v, passportRequiredFields)->isPresent)
->Belt.Array.reduce(0, (a, b) => a + b)
->Js.log

// Part two
