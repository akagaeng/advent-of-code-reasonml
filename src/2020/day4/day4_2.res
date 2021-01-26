// return array of dict

let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n\n")
let fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

let passports =
  inputs->Belt.Array.map(input =>
    Js.String2.split(input, "\n")
    ->Belt.Array.map(f => f->Js.String2.split(" "))
    ->Belt.Array.concatMany
    ->Belt.Array.map(s => s->Js.String2.split(":"))
    ->Belt.Array.map(arr => (arr[0], arr[1]))
    ->Belt.Map.String.fromArray
  )

let excludeCid = arr => arr->Belt.Array.keep(f => f !== "cid")

let hasSameArrayLength = (arrA, arrB) => arrA->Belt.Array.length === arrB->Belt.Array.length

let dictKeysToArray = dict => dict->Belt.Map.String.keysToArray

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

let validateKeyPresent = dict => {
  let keys = dict->dictKeysToArray->excludeCid
  hasSameArrayLength(keys, fields->excludeCid)
}

let validateKeyMatchesRegexp = dict => {
  let matchedKeys = fields
  ->excludeCid
  ->Belt.Array.map(key => {
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
  ->Belt.Array.keep(v => v === true)

  hasSameArrayLength(matchedKeys, fields->excludeCid)
}

let validate = ((dict, validator)) =>
  switch validator {
  | "keyPresent" => (dict, validateKeyPresent(dict))
  | "KeyMatchesRegexp" => (dict, validateKeyMatchesRegexp(dict))
  | _ => (dict, false)
  }->(
    ((dict, isValidated)) => {
      switch isValidated {
      | true => Some(dict)
      | false => None
      }
    }
  )

// Part One
passports
->Belt.Array.map(passport => validate((passport, "keyPresent")))
->Belt.Array.keepMap(v => v)
->Belt.Array.length
->Js.log

// Part two
passports
->Belt.Array.map(p => validate((p, "keyPresent")))
->Belt.Array.keepMap(v => v)
->Belt.Array.map(p => validate((p, "KeyMatchesRegexp")))
->Belt.Array.keepMap(v => v)
->Belt.Array.length
->Js.log
