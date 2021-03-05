let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(input => {
    input
    ->Js.String2.split("\n")
    ->Belt.Array.map(f => f->Js.String2.split(" "))
    ->Belt.Array.concatMany
    ->Belt.Array.map(s => s->Js.String2.split(":"))
    ->Belt.Array.map(arr => (arr[0], arr[1]))
    ->Belt.Map.String.fromArray
  })

type passport_t = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string, // NOT int: case where pid starts with 0 (ex: pid:085002665)
  cid: option<string>,
}

type passports_t = array<passport_t>

let rangeFilter = ((val, min, max)) => val > min - 1 && val < max + 1

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

let regexFilter = ((val: string, regexp)) => {
  let matched = Js.String2.match_(val, regexp)
  switch matched {
  | Some(m) => m[0] === val
  | None => false
  }
}

let parsePassportsRaw = (inputs): passports_t => {
  let optStrToInt = optStr => optStr->Belt.Int.fromString->Belt.Option.getExn
  let getIntValue = (dict, key) => dict->Belt.Map.String.getExn(key)->optStrToInt
  let getStrValue = (dict, key) => dict->Belt.Map.String.getExn(key)
  let getOptValue = (dict, key) => dict->Belt.Map.String.getWithDefault("", key)

  inputs->Belt.Array.keepMap(input => {
    try {
      Some({
        byr: input->getIntValue("byr"),
        iyr: input->getIntValue("iyr"),
        eyr: input->getIntValue("eyr"),
        hgt: input->getStrValue("hgt"),
        hcl: input->getStrValue("hcl"),
        ecl: input->getStrValue("ecl"),
        pid: input->getStrValue("pid"),
        cid: Some(input->getOptValue("cid")),
      })
    } catch {
    | Not_found => None
    | _ => None
    }
  })
}

let parsePassports = (passports: passports_t): passports_t => {
  passports->Belt.Array.keepMap(passport => {
    let isValidated =
      [
        rangeFilter((passport.byr, 1920, 2002)),
        rangeFilter((passport.iyr, 2010, 2020)),
        rangeFilter((passport.eyr, 2020, 2030)),
        hgtFilter((passport.hgt, 150, 193, 59, 76)),
        regexFilter((passport.hcl, %re("/(#)[0-9a-f]{6}/"))),
        regexFilter((passport.ecl, %re("/(amb|blu|brn|gry|grn|hzl|oth)/"))),
        regexFilter((passport.pid, %re("/[0-9]{9}/"))),
      ]->Belt.Array.every(x => x === true)

    switch isValidated {
    | true => Some(passport)
    | _ => None
    }
  })
}

// Part One - refactored
inputs->parsePassportsRaw->Belt.Array.length->Js.log

// Part two - refactored
inputs->parsePassportsRaw->parsePassports->Belt.Array.length->Js.log
