type terminate_t =
  | InfiniteLoop
  | OutOfIndex
  | NotTerminated

type state_t = {
  idx: int,
  value: int,
  terminateState: terminate_t,
  visitIndexes: list<int>,
}

type instruction_t =
  | Acc(int)
  | Jmp(int)
  | Nop

type instructions_t = array<instruction_t>

let initialState: state_t = {idx: 0, value: 0, terminateState: NotTerminated, visitIndexes: list{}}

let convertToSignedNumber = (signValue: string): int =>
  signValue->Js.String2.split(" ")->Belt.Array.map(v => v->int_of_string)->Belt.Array.getExn(0)

let parse = (strs: array<string>): instructions_t => {
  strs->Belt.Array.map(str => {
    let currentLine = str->Js.String2.split(" ")
    let operator = currentLine[0]
    let value = currentLine[1]->convertToSignedNumber

    switch operator {
    | "acc" => Acc(value)
    | "jmp" => Jmp(value)
    | "nop" => Nop
    | _ => raise(Not_found)
    }
  })
}

let isVisited = (thisState: state_t): bool =>
  thisState.visitIndexes->Belt.List.some(visitIndex => visitIndex == thisState.idx)

let terminateCheck = (instructions: instructions_t, thisState: state_t): terminate_t => {
  if thisState->isVisited == true {
    InfiniteLoop
  } else if thisState.idx > instructions->Belt.Array.length - 1 {
    OutOfIndex
  } else {
    NotTerminated
  }
}

let run = (instructions: instructions_t, thisState: state_t): state_t => {
  let terminateState = terminateCheck(instructions, thisState)
  let thisInstruction = instructions->Belt.Array.get(thisState.idx)

  switch terminateState {
  | InfiniteLoop
  | OutOfIndex => {
      ...thisState,
      terminateState: terminateState,
    }
  | NotTerminated =>
    switch thisInstruction {
    | Some(Acc(value)) => {
        idx: thisState.idx + 1,
        value: thisState.value + value,
        terminateState: terminateState,
        visitIndexes: thisState.visitIndexes->Belt.List.add(thisState.idx),
      }
    | Some(Jmp(value)) => {
        ...thisState,
        idx: thisState.idx + value,
        terminateState: terminateState,
        visitIndexes: thisState.visitIndexes->Belt.List.add(thisState.idx),
      }
    | Some(Nop) => {
        ...thisState,
        idx: thisState.idx + 1,
        terminateState: terminateState,
        visitIndexes: thisState.visitIndexes->Belt.List.add(thisState.idx),
      }
    | _ => raise(Not_found)
    }
  }
}

let rec execute = (instructions: instructions_t, thisState: state_t): state_t => {
  switch thisState.terminateState {
  | InfiniteLoop
  | OutOfIndex => thisState
  | NotTerminated => {
      let newState = run(instructions, thisState)
      instructions->execute(newState)
    }
  }
}

let makeCandidates = (instructions: instructions_t): array<instructions_t> => {
  let swap = (instuctions, index, instruction: instruction_t) => {
    let newInstructions = instuctions->Belt.Array.copy
    let _setValue = newInstructions->Belt.Array.set(index, instruction)
    newInstructions
  }

  instructions->Belt.Array.reduceWithIndex([], (acc, x, i) => {
    switch x {
    | Jmp(_) => acc->Belt.Array.concat([instructions->swap(i, Nop)])
    | Nop => acc->Belt.Array.concat([instructions->swap(i, Jmp(i))])
    | _ => acc
    }
  })
}

let onlyTerminatedOutOfIndex = (resultStates: array<state_t>): array<state_t> => {
  resultStates->Belt.Array.keepMap(result => {
    switch result.terminateState {
    | OutOfIndex => Some(result)
    | _ => None
    }
  })
}

// Common
let instructions = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")->parse

// Part 1
let outP1 = instructions->execute(initialState)
outP1.value->Js.log

// Part 2
let outP2 =
  instructions
  ->makeCandidates
  ->Belt.Array.map(candidate => candidate->execute(initialState))
  ->onlyTerminatedOutOfIndex

outP2[0].value->Js.log
