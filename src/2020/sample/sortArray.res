module StrCmp = Belt.Id.MakeComparable({
  type t = string
  let cmp = Pervasives.compare
})

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let sortStringArray = arr => {
  Belt.Set.fromArray(arr, ~id=module(StrCmp))->Belt.Set.toArray
}

["4", "3", "1", "2"]->sortStringArray->Js.log

let sortStringArray = arr => {
  Belt.Set.fromArray(arr, ~id=module(IntCmp))->Belt.Set.toArray
}

[4, 3, 1, 2]->sortStringArray->Js.log
