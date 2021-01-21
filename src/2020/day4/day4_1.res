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

let validateKeyPresent = dict => {
  passportRequiredFields->Belt.Array.every(key => {
    let val = dict->Belt.Map.String.get(key)
    switch val {
    | None => false
    | _ => true
    }
  })
}

// Part One
passports
->Belt.Array.map(passport => passport->validateKeyPresent)
->Belt.Array.keep(v => v === true)
->Belt.Array.length
->Js.log

// Part two
