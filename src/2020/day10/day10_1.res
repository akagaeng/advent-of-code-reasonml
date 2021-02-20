// output joltage:
type number_t = int
type numbers_t = array<number_t>
type adapter_t = {
  one: array<int>,
  two: array<int>,
  three: array<int>,
}
type state_t = {
  currentIndex: int,
  currentValue: int,
  adapters: adapter_t,
}

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let sortInt = (arrs: numbers_t): numbers_t =>
  arrs->Belt.Set.fromArray(~id=module(IntCmp))->Belt.Set.toArray

let jolts: numbers_t =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(s => s->Belt.Int.fromString->Belt.Option.getExn)
  ->sortInt

let rec link = (jolts: numbers_t, state: state_t): state_t => {
  if state.currentIndex > jolts->Belt.Array.length - 1 {
    state
  } else {
    let thisJolt = state.currentValue
    let nextJolt = jolts[state.currentIndex]

    switch nextJolt - thisJolt {
    | 1 =>
      jolts->link({
        currentIndex: state.currentIndex + 1,
        currentValue: nextJolt,
        adapters: {
          ...state.adapters,
          one: state.adapters.one->Belt.Array.concat([thisJolt]),
        },
      })
    | 2 =>
      jolts->link({
        currentIndex: state.currentIndex + 1,
        currentValue: nextJolt,
        adapters: {
          ...state.adapters,
          two: state.adapters.two->Belt.Array.concat([thisJolt]),
        },
      })
    | 3 =>
      jolts->link({
        currentIndex: state.currentIndex + 1,
        currentValue: nextJolt,
        adapters: {
          ...state.adapters,
          three: state.adapters.three->Belt.Array.concat([thisJolt]),
        },
      })
    | _ => state
    }
  }
}

let summarizeAdapters = (state: state_t): int => {
  let oneLen = state.adapters.one->Belt.Array.length
  let threeLen = state.adapters.three->Belt.Array.length
  oneLen * (threeLen + 1)
}

let initState: state_t = {
  currentIndex: 0,
  currentValue: 0,
  adapters: {
    one: [],
    two: [],
    three: [],
  },
}

jolts->link(initState)->summarizeAdapters->Js.log
