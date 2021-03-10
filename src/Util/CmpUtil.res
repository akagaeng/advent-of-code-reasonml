module StrCmp = Belt.Id.MakeComparable({
  type t = string
  let cmp = Pervasives.compare
})

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let intArrToSet = (arr: array<int>) => arr->Belt.Set.fromArray(~id=module(IntCmp))
let strArrToSet = (arr: array<string>) => arr->Belt.Set.fromArray(~id=module(StrCmp))

let sortIntArray = (arr: array<int>): array<int> => arr->intArrToSet->Belt.Set.toArray
let sortStrArray = (arr: array<string>): array<string> => arr->strArrToSet->Belt.Set.toArray

let getMax = (arr: array<int>): int => arr->intArrToSet->Belt.Set.maximum->Belt.Option.getExn
let getMin = (arr: array<int>): int => arr->intArrToSet->Belt.Set.minimum->Belt.Option.getExn
