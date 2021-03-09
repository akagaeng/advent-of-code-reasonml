type instruction = int

type instructions = array<instruction>

type state = {idx: int, steps: int, instructions: instructions}

let initialInstructions =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(v => v->int_of_string)

let initialState = {idx: 0, steps: 0, instructions: initialInstructions}

let makeInstructions = (state: state, idx: int, instruction: instruction) => {
  let newInstructions = state.instructions->Belt.Array.copy
  let _setValue = newInstructions->Belt.Array.set(idx, instruction)
  newInstructions
}

let move = (offsetUpdater, state: state): state => {
  idx: state.idx + state.instructions[state.idx],
  steps: state.steps + 1,
  instructions: offsetUpdater(state),
}

let offsetUpdaterPart1 = (state: state): instructions => {
  let offset = state.instructions[state.idx]
  state->makeInstructions(state.idx, offset + 1)
}

let offsetUpdaterPart2 = (state: state): instructions => {
  let offset = state.instructions[state.idx]
  switch offset >= 3 {
  | true => state->makeInstructions(state.idx, offset - 1)
  | false => state->makeInstructions(state.idx, offset + 1)
  }
}

let getSteps = (state: state): int => state.steps

let isTerminated = (state): bool => state.idx >= state.instructions->Belt.Array.length

let terminateCheck = Checker.check(isTerminated)

let p1move = move(offsetUpdaterPart1)
let p2move = move(offsetUpdaterPart2)

let p1check = terminateCheck(p1move)
let p2check = terminateCheck(p2move)

// Part 1
p1check(initialState)->getSteps->Js.log

// Part 2
p2check(initialState)->getSteps->Js.log
