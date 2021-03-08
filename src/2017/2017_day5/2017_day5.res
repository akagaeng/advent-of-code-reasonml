type instruction = int

type instructions = array<instruction>

type state = {idx: int, steps: int}

let initialState = {idx: 0, steps: 0}

let initialInstructions =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(v => v->int_of_string)

let run = (instructions: instructions, idx: int, instruction: instruction) => {
  let newInstructions = instructions->Belt.Array.copy
  let _setValue = newInstructions->Belt.Array.set(idx, instruction)
  newInstructions
}

let move = (instructions: instructions, state: state, offsetUpdater) => {
  let offset = instructions[state.idx]
  (instructions->offsetUpdater(state), {idx: state.idx + offset, steps: state.steps + 1})
}

let rec check = (instructions: instructions, state: state, offsetUpdater) => {
  switch state.idx < instructions->Belt.Array.length {
  | true => {
      let (newInstructions, thisState) = instructions->move(state, offsetUpdater)
      newInstructions->check(thisState, offsetUpdater)
    }
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
initialInstructions->check(initialState, offsetUpdaterPart1)->getSteps->Js.log

// Part 2
initialInstructions->check(initialState, offsetUpdaterPart2)->getSteps->Js.log
