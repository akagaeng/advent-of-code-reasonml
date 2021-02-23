type terminate_t =
  | InfiniteLoop // infinite loop
  | OutOfIndex // out of index
  | NotYet // not terminated

type state_t = {
  idx: int,
  value: int,
  terminateState: terminate_t,
  visitIndexes: array<int>,
}

type instruction_t =
  | Acc(int)
  | Jmp(int)
  | Nop

type instructions_t = array<instruction_t>

let initialState: state_t = {idx: 0, value: 0, terminateState: NotYet, visitIndexes: []}

let splitSignValue = signValue => {
  let re = %re("/(\+|\-)([0-9]+)/")
  let result = Js.Re.exec_(re, signValue)

  switch result {
  | Some(r) => {
      let sign = Js.Nullable.toOption(Js.Re.captures(r)[1])
      let value =
        Js.Nullable.toOption(Js.Re.captures(r)[2])
        ->Belt.Option.getExn
        ->Belt.Int.fromString
        ->Belt.Option.getExn

      if sign == Some("-") {
        Some(-value)
      } else {
        Some(value)
      }
    }
  | None => None
  }
}

let parse = (strs: array<string>): instructions_t => {
  strs->Belt.Array.mapWithIndex((_, str) => {
    let line = str->Js.String2.split(" ")
    let (operator, signValue) = (line[0], line[1])
    let value = splitSignValue(signValue)->Belt.Option.getExn

    switch operator {
    | "acc" => Acc(value)
    | "jmp" => Jmp(value)
    | "nop" => Nop
    | _ => raise(Not_found)
    }
  })
}

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let includes = (arrs, el) => Belt.Set.fromArray(arrs, ~id=module(IntCmp))->Belt.Set.has(el)

// let isTerminated = (originalInstructions: instructions_t, thisState: state_t): terminate_t => {
//   let instructions = originalInstructions->Belt.Array.copy
//   if isVisited(thisState) {
//     Inf({
//       ...thisState,
//       visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.idx]),
//     })
//   } else if thisState.idx == instructions->Belt.Array.length - 1 {
//     Ooi({
//       ...thisState,
//       visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.idx]),
//     })
//   } else {
//     No(thisState)
//   }
// }

// let run ~

// let rec run = s => {
//   let s' = operate(s)
//   switch s'.terminalState {
//   | Inf => ""
//   | Ooi => ""
//   | No => run(s')
// }

let isVisited = (thisState): bool => {
  thisState.visitIndexes->Belt.Array.some(visitIndex => {visitIndex == thisState.idx}) == true
}

let terminateCheck = (instructions, thisState: state_t): terminate_t => {
  if thisState->isVisited == true {
    InfiniteLoop
  } else if thisState.idx == instructions->Belt.Array.length - 1 {
    OutOfIndex
  } else {
    NotYet
  }
}

let run = (instructions: instructions_t, thisState: state_t): state_t => {
  let thisInstruction = instructions->Belt.Array.getExn(thisState.idx)

  let terminateState = terminateCheck(instructions, thisState)
  //

  switch thisInstruction {
  | Acc(value) => {
      idx: thisState.idx + 1,
      value: thisState.value + value,
      terminateState: terminateState,
      visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.idx]),
    }
  | Jmp(value) => {
      ...thisState,
      idx: thisState.idx + value,
      terminateState: terminateState,
      visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.idx]),
    }
  | Nop => {
      ...thisState,
      idx: thisState.idx + 1,
      terminateState: terminateState,
      visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.idx]),
    }
  }
}

let rec execute = (originalInstructions: instructions_t, thisState: state_t): state_t => {
  let instructions = originalInstructions->Belt.Array.copy
  // let thisInstruction = instructions->Belt.Array.getExn(thisState.idx)

  // Js.log(("thisState:", thisState))
  let newState = run(instructions, thisState)
  // Js.log((">>newState:", newState))

  switch newState.terminateState {
  | InfiniteLoop
  | OutOfIndex => thisState
  | NotYet => instructions->execute(newState) // newState  not terminated
  }
}

// switch instructions->isTerminated(thisState) {
// | Inf(st) => raise(InfiniteLoop(Inf(st)))
// | Ooi(st) => raise(OutOfIndex(Ooi(st)))
// | No(st) =>
//   switch thisInstruction {
//   | Acc(el) => {
//       idx: st.idx + 1,
//       val: st.val + el.value,
//       visitIndexes: st.visitIndexes->Belt.Array.concat([st.idx]),
//     }
//   | Jmp(el) => {
//       ...thisState,
//       idx: st.idx + el.value,
//       visitIndexes: st.visitIndexes->Belt.Array.concat([st.idx]),
//     }
//   | Nop => {
//       ...thisState,
//       idx: st.idx + 1,
//       visitIndexes: st.visitIndexes->Belt.Array.concat([st.idx]),
//     }
//   }
// }
// }

let findInstructionWithNopJmp = instructions =>
  instructions->Belt.Array.keep(instruction => {
    switch instruction {
    | Acc(_) => false
    | Jmp(_) => true
    | Nop => true
    }
  })

// Common
let originalInstructions = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n")->parse

// let instructionWithNopJmps = originalInstructions->findInstructionWithNopJmp

// Part 1
let outP1 = originalInstructions->execute(initialState)
outP1.value->Js.log

// try {
//   let _ = originalInstructions->operate(initialState)
//   Some(initialState)
// } catch {
// | InfiniteLoop(Inf(state)) => Some(state)
// | OutOfIndex(Ooi(state)) => Some(state)
// | _ => None
// }

// let finalStateP1 = originalInstructions->operate(initialState)
// finalStateP1.val->Js.log

// Part 2
// let replaceIndex = instructionWithNopJmps
// ->Belt.Array.map(v => v.index)
// replaceIndex->Js.log

// let filterLastIndexError = finalStateP2 =>
//   finalStateP2
//   ->Belt.Array.keep(s => {
//     switch s {
//     | Inf(_) => true
//     | Ooi(_) => true
//     | _ => false
//     }
//   })
//   ->Belt.Array.getExn(0)

// let finalStateP2 = replaceIndex
// ->Belt.Array.map(replaceAt => {
//   let instructions = originalInstructions->replace(replaceAt)
//   instructions->operate(initialState)
// })
// ->filterLastIndexError

// finalStateP2.val->Js.log

// let switchNopJmp

// let findNopJmp = (instructions: instructions_t): instructions_t => {
//   instructions->Belt.Array.keepMap(instr => {
//     switch instr {
//     | Nop(_) => Some(instr)
//     | Jmp(_) => Some(instr)
//     | _ => None
//     }
//   })
// }

let makeCandidates = (originalInstructions: instructions_t): array<instructions_t> => {
  let instructions = originalInstructions->Belt.Array.copy
  // let replaceDone = instructions->Belt.Array.set()

  let jmpNopInstructions = instructions->Belt.Array.getBy(instruction => {
    instruction == Jmp(_) || instruction == Nop
  })

  jmpNopInstructions

  // jmpNopInstructions->Belt.Array.map(jmpNopInstruction => jmpNopInstruction)
}

let outP2 = originalInstructions->makeCandidates
outP2->Js.log

// let candidateInstructionsArr = originalInstructions->makeCandidates(state)
// candidateInstructionsArr->Js.log

// let outP2 = candidateInstructionsArr->Belt.Array.map(candidateInstructions => {
//   try {
//     candidateInstructions->operate(initialState)
//   } catch {
//   | InfiniteLoop(Inf(state)) =>
//     candidateInstructions->operate({
//       ...state,
//       idx: state.idx + 1,
//     })
//   | OutOfIndex(Ooi(state)) => {
//       state
//     }
//   }
// })

// let filterLastIndexError = finalStateP2 =>
//   finalStateP2
//   ->Belt.Array.keep(s => {
//     switch s {
//     | Inf(_) => true
//     | Ooi(_) => true
//     | _ => false
//     }
//   })
//   ->Belt.Array.getExn(0)

// outP2->Js.log
// ->filterLastIndexError

// candidateInstructionsArr->Js.log
