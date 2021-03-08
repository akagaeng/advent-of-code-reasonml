type instruction = int

type instructions = array<instruction>

type state = {idx: int, steps: int, instructions: instructions}

let initialInstructions =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(v => v->int_of_string)

let initialState = {idx: 0, steps: 0, instructions: initialInstructions}

let run = (instructions: instructions, idx: int, instruction: instruction) => {
  let newInstructions = instructions->Belt.Array.copy
  let _setValue = newInstructions->Belt.Array.set(idx, instruction)
  newInstructions
}

let move = (state: state, offsetUpdater) => {
  idx: state.idx + state.instructions[state.idx],
  steps: state.steps + 1,
  instructions: state.instructions->offsetUpdater(state),
}

let rec check = (state: state, offsetUpdater) => {
  switch state.idx < state.instructions->Belt.Array.length {
  | true => check(move(state, offsetUpdater), offsetUpdater)
  | false => state
  }
}

let offsetUpdaterPart1 = (instructions, state): instructions => {
  let offset = instructions[state.idx]
  instructions->run(state.idx, offset + 1)
}

let offsetUpdaterPart2 = (instructions, state): instructions => {
  let offset = instructions[state.idx]
  switch offset >= 3 {
  | true => instructions->run(state.idx, offset - 1)
  | false => instructions->run(state.idx, offset + 1)
  }
}

let getSteps = (state: state): int => state.steps

// Part 1
check(initialState, offsetUpdaterPart1)->getSteps->Js.log

// Part 2
check(initialState, offsetUpdaterPart2)->getSteps->Js.log
