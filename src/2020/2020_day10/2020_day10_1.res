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
  chainCount: int,
  subSum: int,
  acc: int,
}

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let sortInt = (arrs: numbers_t): numbers_t =>
  arrs->Belt.Set.fromArray(~id=module(IntCmp))->Belt.Set.toArray

let jolts: numbers_t =
  Node.Fs.readFileAsUtf8Sync("./sample2.txt")
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
          one: state.adapters.one->Belt.Array.concat([nextJolt]),
        },
      })
    | 2 =>
      jolts->link({
        currentIndex: state.currentIndex + 1,
        currentValue: nextJolt,
        adapters: {
          ...state.adapters,
          two: state.adapters.two->Belt.Array.concat([nextJolt]),
        },
      })
    | 3 =>
      jolts->link({
        currentIndex: state.currentIndex + 1,
        currentValue: nextJolt,
        adapters: {
          ...state.adapters,
          three: state.adapters.three->Belt.Array.concat([nextJolt]),
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

let equation = (length: int): int =>
  Js.Math.pow_float(~base=2.0, ~exp=(length + 1)->Belt.Int.toFloat)->Belt.Int.fromFloat

let rec checkVariations = (ones: numbers_t, s: calcState_t): calcState_t => {
  let oneMaxIdx = ones->Belt.Array.length - 1
  let thisIndex = s.currentIndex
  let nextIndex = s.currentIndex + 1
  let currentValue = ones[thisIndex]
  let nextValue = ones[nextIndex]

  let difference = nextValue - currentValue

  if oneMaxIdx == nextIndex {
    let sum = equation(s.chainCount)
    {
      ...s,
      acc: s.acc == 0 ? sum : s.acc + s.acc * sum,
    }
  } else if difference == 1 {
    ones->checkVariations({
      ...s,
      currentIndex: s.currentIndex + 1,
      chainCount: s.chainCount + 1,
      subSum: {
        if s.chainCount > 0 {
          s.subSum == 0 ? 2 : s.subSum * 2
        } else {
          0
        }
      },
    })
  } else {
    ones->checkVariations({
      currentIndex: s.currentIndex + 1,
      chainCount: 0,
      subSum: 0,
      acc: {
        if s.chainCount > 1 {
          let sum = equation(s.chainCount)
          s.acc == 0 || sum == 0 ? sum : s.acc + s.acc * sum
        } else {
          s.acc
        }
      },
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
  chainCount: 0,
  subSum: 0,
  acc: 0,
}

let joltFinalState = jolts->link(initState)

// Part 1
joltFinalState->summarizeAdapters->Js.log

// Part 2
let ones = joltFinalState.adapters.one
ones->checkVariations(initialCalcState)->Js.log
