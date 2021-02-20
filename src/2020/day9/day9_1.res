type number_t = int
type numbers_t = array<number_t>
type state_t = {
  currentIndex: int,
  length: int,
  errors: array<number_t>,
}

let inputs: numbers_t =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(s => s->Belt.Int.fromString->Belt.Option.getExn)

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let sumOfTwo = (arrs: numbers_t): numbers_t =>
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

let rec search = (inputs: numbers_t, state: state_t): state_t => {
  if state.currentIndex == inputs->Belt.Array.length - state.length {
    state
  } else {
    let thisPreamble = inputs->Belt.Array.slice(~offset=state.currentIndex, ~len=state.length)
    let thisValue = inputs[state.length + state.currentIndex]

    switch thisPreamble->sumOfTwo->has(thisValue) {
    | false =>
      inputs->search({
        ...state,
        currentIndex: state.currentIndex + 1,
        errors: state.errors->Belt.Array.concat([thisValue]),
      })
    | true =>
      inputs->search({
        ...state,
        currentIndex: state.currentIndex + 1,
      })
    }
  }
}

let initialStart = 25
let initialState = {currentIndex: 0, length: initialStart, errors: []}
let finalState = inputs->search(initialState)

// Part 1
finalState.errors[0]->Js.log
