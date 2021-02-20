type number_t = int
type numbers_t = array<number_t>
type state_t = {
  currentIndex: int,
  length: int,
  founds: array<number_t>,
}

type contiguousSet_t = array<number_t>

let inputs: numbers_t =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(s => s->Belt.Int.fromString->Belt.Option.getExn)

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let sortInt = (arrs: numbers_t): numbers_t =>
  arrs->Belt.Set.fromArray(~id=module(IntCmp))->Belt.Set.toArray

let sum2D = (arrs: numbers_t): numbers_t =>
  arrs
  ->Belt.Array.map(vo => {
    arrs->Belt.Array.map(vi => {
      vo + vi
    })
  })
  ->Belt.Array.concatMany
  ->Belt.Set.fromArray(~id=module(IntCmp))
  ->Belt.Set.toArray

let has = (arrs: numbers_t, value: number_t): bool => {
  arrs->Belt.Array.some(x => x == value)
}

let rec findErrors = (inputs: numbers_t, state: state_t): state_t => {
  if state.currentIndex == inputs->Belt.Array.length - state.length {
    state
  } else {
    let thisPreamble = inputs->Belt.Array.slice(~offset=state.currentIndex, ~len=state.length)
    let thisValue = inputs[state.length + state.currentIndex]

    switch thisPreamble->sum2D->has(thisValue) {
    | false =>
      inputs->findErrors({
        ...state,
        currentIndex: state.currentIndex + 1,
        founds: state.founds->Belt.Array.concat([thisValue]),
      })
    | true =>
      inputs->findErrors({
        ...state,
        currentIndex: state.currentIndex + 1,
      })
    }
  }
}

let sumAll = (ints: numbers_t) => ints->Belt.Array.reduce(0, (acc, item) => acc + item)

let sumMinMax = (arrs: numbers_t): number_t => {
  let sortedArrs = arrs->sortInt
  let lastIdx = sortedArrs->Belt.Array.length - 1
  sortedArrs[0] + sortedArrs[lastIdx]
}

let rec findWeakness = (inputs: numbers_t, state: state_t, targetValue: int): state_t => {
  if state.currentIndex == inputs->Belt.Array.length - 1 {
    inputs->findWeakness(
      {
        ...state,
        currentIndex: 0,
        length: state.length + 1,
      },
      targetValue,
    )
  } else {
    let contiguousSet = inputs->Belt.Array.slice(~offset=state.currentIndex, ~len=state.length)

    switch contiguousSet->sumAll == targetValue {
    | false =>
      inputs->findWeakness(
        {
          ...state,
          currentIndex: state.currentIndex + 1,
        },
        targetValue,
      )
    | true => {
        ...state,
        founds: state.founds->Belt.Array.concat(contiguousSet),
      }
    }
  }
}

// Part 1
let initialStateP1 = {currentIndex: 0, length: 25, founds: []}
let part1FinalState = inputs->findErrors(initialStateP1)
let part1Answer = part1FinalState.founds[0]
part1Answer->Js.log

// Part 2
let initialStateP2 = {currentIndex: 0, length: 2, founds: []}
let part2FinalState = inputs->findWeakness(initialStateP2, part1Answer)
let part2FinalFounds = part2FinalState.founds
part2FinalFounds->sumMinMax->Js.log
