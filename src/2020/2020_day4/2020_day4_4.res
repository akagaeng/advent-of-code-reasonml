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

type raw_t = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

type raws_t = array<raw_t>

type passport_t = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

type passports_t = array<passport_t>

let rangeFilter = (val, (min, max)) => val > min - 1 && val < max + 1

let hgtFilter = (val, (cmMin, cmMax, inMin, inMax)) => {
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
      | "cm" => num->rangeFilter((cmMin, cmMax))
      | "in" => num->rangeFilter((inMin, inMax))
      | _ => false
      }
    }
  }
}

let regexFilter = (val: string, regexp) => {
  let matched = Js.String2.match_(val, regexp)
  switch matched {
  | Some(m) => m[0] === val
  | None => false
  }
}

let optStrToInt = optStr => optStr->Belt.Int.fromString->Belt.Option.getExn

let parseRaws = (inputs): raws_t => {
  let getStrValue = (dict, key: string): string => dict->Belt.Map.String.getExn(key)
  let getOptValue = (dict, key) => dict->Belt.Map.String.get(key)

  inputs->Belt.Array.keepMap(input => {
    try {
      let rasPassport: raw_t = {
        byr: input->getStrValue("byr"),
        iyr: input->getStrValue("iyr"),
        eyr: input->getStrValue("eyr"),
        hgt: input->getStrValue("hgt"),
        hcl: input->getStrValue("hcl"),
        ecl: input->getStrValue("ecl"),
        pid: input->getStrValue("pid"),
        cid: input->getOptValue("cid"),
      }

      Some(rasPassport)
    } catch {
    | Not_found => None
    | _ => None
    }
  })
}

let parsePassports = (raws: raws_t): passports_t => {
  raws->Belt.Array.keepMap(raw => {
    let passport: passport_t = {
      byr: raw.byr->optStrToInt,
      iyr: raw.iyr->optStrToInt,
      eyr: raw.eyr->optStrToInt,
      hgt: raw.hgt,
      hcl: raw.hcl,
      ecl: raw.ecl,
      pid: raw.pid,
      cid: raw.cid,
    }

    let isValidated =
      [
        passport.byr->rangeFilter((1920, 2002)),
        passport.iyr->rangeFilter((2010, 2020)),
        passport.eyr->rangeFilter((2020, 2030)),
        passport.hgt->hgtFilter((150, 193, 59, 76)),
        passport.hcl->regexFilter(%re("/(#)[0-9a-f]{6}/")),
        passport.ecl->regexFilter(%re("/(amb|blu|brn|gry|grn|hzl|oth)/")),
        passport.pid->regexFilter(%re("/[0-9]{9}/")),
      ]->Belt.Array.every(x => x === true)

    switch isValidated {
    | true => Some(passport)
    | _ => None
    }
  })
}

let countRaw: raws_t => int = a => a->Belt.Array.length

let countPassports: passports_t => int = a => a->Belt.Array.length

// Part One - refactored
inputs->parseRaws->countRaw->Js.log

// Part two - refactored
inputs->parseRaws->parsePassports->countPassports->Js.log
