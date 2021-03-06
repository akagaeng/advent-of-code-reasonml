type instruction =
  | Stopped
  | Jump(int) // Forward + Backward

type instructions = array<instruction>

type state = {idx: int, steps: int}

let initialState = {idx: 0, steps: 0}

let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(v => v->int_of_string)

let parse = (inputs): instructions => {
  inputs->Belt.Array.map(input => {
    switch input == 0 {
    | true => Stopped
    | false => Jump(input)
    }
  })
}

let initialInstructions = inputs->parse

let updateOffset = (instructions: instructions, idx: int, instruction: instruction) => {
  let _setValue = instructions->Belt.Array.set(idx, instruction)
  instructions
}

let move = (instructions: instructions, state: state) => {
  let thisIdx = state.idx
  let thisInstruction = instructions[thisIdx]

  switch thisInstruction {
  | Stopped => {
      let newInstructions = instructions->updateOffset(thisIdx, Jump(1))
      (newInstructions, {idx: thisIdx, steps: state.steps + 1})
    }
  | Jump(offset) =>
    switch thisIdx + 1 {
    | 0 => {
        // Jump -> Stopped
        let newInstructions = instructions->updateOffset(thisIdx, Stopped)
        (newInstructions, {idx: thisIdx + offset, steps: state.steps + 1})
      }
    | _ => {
        // Jump -> Jump
        let newInstructions = instructions->updateOffset(thisIdx, Jump(offset + 1))
        (newInstructions, {idx: thisIdx + offset, steps: state.steps + 1})
      }
    }
  }
}

let rec check = (instructions: instructions, state: state) => {
  switch state.idx < instructions->Belt.Array.length {
  | true => {
      let (newInstructions, thisState) = instructions->move(state)
      newInstructions->check(thisState)
    }
  | false => state
  }
}

// Part 1
initialInstructions->check(initialState)->Js.log

/*
Instructions
- The goal is to follow the jumps until one leads outside the list.
- after each jump, the offset of that instruction increases by 1
*/
