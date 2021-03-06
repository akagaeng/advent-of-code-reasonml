type instruction =
  | Stopped
  | Jump(int) // Forward + Backward

type rule =
  | Part1
  | Part2

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
  let newInstructions = instructions->Belt.Array.copy
  let _setValue = newInstructions->Belt.Array.set(idx, instruction)
  newInstructions
}

let moveP1 = (instructions: instructions, state: state) => {
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

let moveP2 = (instructions: instructions, state: state) => {
  let thisIdx = state.idx
  let thisInstruction = instructions[thisIdx]

  switch thisInstruction {
  | Stopped => {
      let newInstructions = instructions->updateOffset(thisIdx, Jump(1))
      (newInstructions, {idx: thisIdx, steps: state.steps + 1})
    }
  | Jump(offset) =>
    if offset >= 3 {
      switch thisIdx + 1 {
      | 0 => {
          // Jump -> Stopped
          let newInstructions = instructions->updateOffset(thisIdx, Stopped)
          (newInstructions, {idx: thisIdx + offset, steps: state.steps + 1})
        }
      | _ => {
          // Jump -> Jump
          let newInstructions = instructions->updateOffset(thisIdx, Jump(offset - 1))
          (newInstructions, {idx: thisIdx + offset, steps: state.steps + 1})
        }
      }
    } else {
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
}

let rec check = (instructions: instructions, state: state, rule: rule) => {
  switch state.idx < instructions->Belt.Array.length {
  | true =>
    switch rule {
    | Part1 => {
        let (newInstructions, thisState) = instructions->moveP1(state)
        newInstructions->check(thisState, Part1)
      }
    | Part2 => {
        let (newInstructions, thisState) = instructions->moveP2(state)
        newInstructions->check(thisState, Part2)
      }
    }
  | false => state
  }
}

// Part 1
initialInstructions->check(initialState, Part1)->Js.log

// Part 2
initialInstructions->check(initialState, Part2)->Js.log
