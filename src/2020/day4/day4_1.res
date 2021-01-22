// array of array

let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n\n")
let fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
let passportRequiredFields = fields->Belt.Array.keep(f => f !== "cid")

let passports =
  inputs->Belt.Array.map(input =>
    input
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

let rangeFilter = ((val, least, most)) => {
  let v = val->Belt.Int.fromString->Belt.Option.getWithDefault(-1)
  switch v {
  | v when v > least - 1 && v < most + 1 => true
  | _ => false
  }
}

let hgtFilter = val => {
  switch val {
  | "" => false
  | _ => {
      let num =
        Js.String2.splitByRe(val, %re("/(cm|in)/"))[0]
        ->Belt.Option.getExn
        ->Belt.Int.fromString
        ->Belt.Option.getExn
      let unit = Js.String2.splitByRe(val, %re("/[0-9]+/"))[1]->Belt.Option.getExn

      switch unit {
      | "cm" when num > 150 - 1 && num < 193 + 1 => true
      | "in" when num > 59 - 1 && num < 76 + 1 => true
      | _ => false
      }
    }
  }
}

let regexFilter = ((val, regexp)) => {
  let matched = Js.String2.match_(val, regexp)
  switch matched {
  | Some(m) => m[0] === val
  | None => false
  }
}

let validateKeyRegexp = dict => {
  passportRequiredFields->Belt.Array.every(key => {
    let val = dict->Belt.Map.String.get(key)->Belt.Option.getWithDefault("")

    switch key {
    | "byr" => rangeFilter((val, 1920, 2002))
    | "iyr" => rangeFilter((val, 2010, 2020))
    | "eyr" => rangeFilter((val, 2020, 2030))
    | "hgt" => hgtFilter(val)
    | "hcl" => regexFilter((val, %re("/(#)[0-9a-f]{6}/")))
    | "ecl" => regexFilter((val, %re("/(amb|blu|brn|gry|grn|hzl|oth)/")))
    | "pid" => regexFilter((val, %re("/[0-9]{9}/")))
    | _ => false
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
passports
->Belt.Array.map(passport => passport->validateKeyRegexp)
->Belt.Array.keep(v => v === true)
->Belt.Array.length
->Js.log
