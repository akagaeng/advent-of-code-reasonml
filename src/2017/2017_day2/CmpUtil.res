module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let arrToSet = (arr: array<int>) => arr->Belt.Set.fromArray(~id=module(IntCmp))

let sortIntArray = (arr: array<int>): array<int> => arr->arrToSet->Belt.Set.toArray

let getMax = (arr: array<int>): int => arr->arrToSet->Belt.Set.maximum->Belt.Option.getExn

let getMin = (arr: array<int>): int => arr->arrToSet->Belt.Set.minimum->Belt.Option.getExn
