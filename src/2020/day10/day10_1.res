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

type calcState_t = {
  currentIndex: int,
  currentValue: int,
  chainCount: int,
}

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let sortInt = (arrs: numbers_t): numbers_t =>
  arrs->Belt.Set.fromArray(~id=module(IntCmp))->Belt.Set.toArray

let jolts: numbers_t =
  Node.Fs.readFileAsUtf8Sync("./sample.txt")
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

let equation = (length: int): float =>
  Js.Math.pow_float(~base=2.0, ~exp=(length - 2)->Belt.Int.toFloat)

let rec checkVariations = (ones: numbers_t, s: calcState_t): calcState_t => {
  let oneMaxIdx = ones->Belt.Array.length - 1
  let nextIndex = s.currentIndex + 1
  let nextValue = ones[nextIndex]
  let difference = nextValue - s.currentValue
  Js.log(("oneMaxIdx", oneMaxIdx, "nextV", nextValue, "difference:", difference))
  Js.log(("state:", s))
  if oneMaxIdx == nextIndex {
    s
  } else if difference == 1 {
    ones->checkVariations({
      currentValue: nextValue,
      currentIndex: s.currentIndex + 1,
      chainCount: s.chainCount + 1,
    })
  } else if s.chainCount > 0 {
    ones->checkVariations({
      currentIndex: s.currentIndex + 1,
      currentValue: s.currentValue + equation(s.chainCount)->Belt.Int.fromFloat,
      chainCount: 0,
    })
  } else {
    ones->checkVariations({
      ...s,
      currentIndex: s.currentIndex + 1,
      chainCount: 0,
    })
  }
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

let initialCalcState: calcState_t = {
  currentIndex: 0,
  currentValue: 0,
  chainCount: 0,
}

let joltFinalState = jolts->link(initState)

// Part 1
// joltFinalState->summarizeAdapters->Js.log

// Part 2
let ones = joltFinalState.adapters.one

ones->Js.log
ones->checkVariations(initialCalcState)->Js.log

/*
  n자리 연속 -> 2^(n-2)
  5자리 연속 -> 8개 variation
  4자리 연속 -> 4개 variation
  3자리 연속 -> 2개 variation
  최종 variation = 개별 variation끼리 곱해줌
*/
