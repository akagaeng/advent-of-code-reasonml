module StrCmp = Belt.Id.MakeComparable({
  type t = string
  let cmp = Pervasives.compare
})

let sortArray = arr => {
  Belt.Set.fromArray(arr, ~id=module(StrCmp))->Belt.Set.toArray
}

["4", "3", "1", "2"]->sortArray->Js.log