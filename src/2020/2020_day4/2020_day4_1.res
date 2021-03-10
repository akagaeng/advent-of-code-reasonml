// return array of boolean

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

let rangeFilter = ((val, min, max)) => {
  switch val {
  | v when v > min - 1 && v < max + 1 => true
  | _ => false
  }
}

let hgtFilter = ((val, cmMin, cmMax, inMin, inMax)) => {
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
      | "cm" => rangeFilter((num, cmMin, cmMax))
      | "in" => rangeFilter((num, inMin, inMax))
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

let strToInt = str => str->Belt.Int.fromString->Belt.Option.getWithDefault(-1)

let validateKeyRegexp = dict => {
  passportRequiredFields->Belt.Array.every(key => {
    let val = dict->Belt.Map.String.get(key)->Belt.Option.getWithDefault("")

    switch key {
    | "byr" => rangeFilter((val->strToInt, 1920, 2002))
    | "iyr" => rangeFilter((val->strToInt, 2010, 2020))
    | "eyr" => rangeFilter((val->strToInt, 2020, 2030))
    | "hgt" => hgtFilter((val, 150, 193, 59, 76))
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
