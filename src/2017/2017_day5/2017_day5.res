type rule =
  | Part1
  | Part2

type instruction = int

type instructions = array<instruction>

type state = {idx: int, steps: int}

let initialState = {idx: 0, steps: 0}

let initialInstructions =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(v => v->int_of_string)

let updateOffset = (instructions: instructions, idx: int, instruction: instruction) => {
  let newInstructions = instructions->Belt.Array.copy
  let _setValue = newInstructions->Belt.Array.set(idx, instruction)
  newInstructions
}

let move = (instructions: instructions, state: state, rule: rule) => {
  let thisIdx = state.idx
  let offset = instructions[thisIdx]

  if offset >= 3 && rule === Part2 {
    let newInstructions = instructions->updateOffset(thisIdx, offset - 1)
    (newInstructions, {idx: thisIdx + offset, steps: state.steps + 1})
  } else {
    let newInstructions = instructions->updateOffset(thisIdx, offset + 1)
    (newInstructions, {idx: thisIdx + offset, steps: state.steps + 1})
  }
}

let rec check = (instructions: instructions, state: state, rule: rule) => {
  switch state.idx < instructions->Belt.Array.length {
  | true => {
      let (newInstructions, thisState) = instructions->move(state, rule)
      newInstructions->check(thisState, rule)
    }
  | false => state
  }
}

let getSteps = (state: state): int => state.steps

// Part 1
initialInstructions->check(initialState, Part1)->getSteps->Js.log

// Part 2
initialInstructions->check(initialState, Part2)->getSteps->Js.log
