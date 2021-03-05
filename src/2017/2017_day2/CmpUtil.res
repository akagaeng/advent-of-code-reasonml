module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let sortIntArray = (arr): array<int> =>
  Belt.Set.fromArray(arr, ~id=module(IntCmp))->Belt.Set.toArray
