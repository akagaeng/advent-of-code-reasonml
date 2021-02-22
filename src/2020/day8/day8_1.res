type state_t = {
  currentIndex: int,
  currentValue: int,
  visitIndexes: array<int>,
  error: string, // "InfiniteLoop" | "LastIndex"
}

// type instruction_t = {
//   index: int,
//   operator: string, // acc, jmp, nop
//   value: int, // w/ sign
// }

type element_t = (int, int)

type instruction_t =
  | Acc(element_t)
  | Jmp(element_t)
  | Nop(element_t)

type instructions_t = array<instruction_t>

exception LastIndex(state_t)
exception InfiniteLoop(state_t)
exception Other

let initialState: state_t = {currentIndex: 0, currentValue: 0, visitIndexes: [], error: ""}

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
  strs->Belt.Array.mapWithIndex((i, str) => {
    let line = str->Js.String2.split(" ")
    let (operator, signValue) = (line[0], line[1])
    let value = splitSignValue(signValue)

    {
      index: i,
      operator: operator,
      value: value->Belt.Option.getExn,
    }
  })
}

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let includes = (arrs, el) => Belt.Set.fromArray(arrs, ~id=module(IntCmp))->Belt.Set.has(el)

let isVisited = thisState => {
  thisState.visitIndexes->Belt.Array.some(visitIndex => {visitIndex == thisState.currentIndex}) ==
    true
}

let rec operate = (originalInstructions: instructions_t, thisState: state_t): state_t => {
  let instructions = originalInstructions->Belt.Array.copy
  let thisInstruction = instructions->Belt.Array.getExn(thisState.currentIndex)

  try {
    if isVisited(thisState) {
      raise(
        InfiniteLoop({
          ...thisState,
          visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.currentIndex]),
          error: "InfiniteLoop",
        }),
      )
    } else if thisState.currentIndex == instructions->Belt.Array.length - 1 {
      raise(
        LastIndex({
          ...thisState,
          visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.currentIndex]),
          error: "LastIndex",
        }),
      )
    } else {
      switch thisInstruction.operator {
      | "nop" =>
        instructions->operate({
          ...thisState,
          currentIndex: thisState.currentIndex + 1,
          visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.currentIndex]),
        })
      | "acc" =>
        instructions->operate({
          ...thisState,
          currentIndex: thisState.currentIndex + 1,
          currentValue: thisState.currentValue + thisInstruction.value,
          visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.currentIndex]),
        })
      | "jmp" =>
        instructions->operate({
          ...thisState,
          currentIndex: thisState.currentIndex + thisInstruction.value,
          visitIndexes: thisState.visitIndexes->Belt.Array.concat([thisState.currentIndex]),
        })
      | _ => thisState
      }
    }
  } catch {
  | InfiniteLoop(thisState) => thisState
  | LastIndex(thisState) => thisState
  | Other => raise(Other)
  }
}

let findInstructionWithNopJmp = instructions =>
  instructions->Belt.Array.keep(instruction => {
    switch instruction.operator {
    | "acc" => false
    | "jmp" => true
    | "nop" => true
    | _ => false
    }
  })

let replace = (originalInstructions: instructions_t, replaceAt: int): instructions_t => {
  let instructions = originalInstructions->Belt.Array.copy
  let replaceDone = instructions->Belt.Array.set(
    replaceAt,
    {
      ...instructions[replaceAt],
      operator: instructions[replaceAt].operator == "nop" ? "jmp" : "nop",
    },
  )

  if replaceDone != true {
    Js.Exn.raiseError("Failed to execute current instruction!")
  }

  instructions
}

let swapNopJmp = (
  instructions: instructions_t,
  instructionWithNopJmps: instructions_t,
  thisState: state_t,
): instructions_t => {
  let indexToSwap = instructionWithNopJmps[thisState.currentIndex].index

  let swapExectution = instructions->Belt.Array.set(
    indexToSwap,
    {
      ...instructions[indexToSwap],
      operator: instructions[indexToSwap].operator == "nop" ? "jmp" : "nop",
    },
  )

  if swapExectution != true {
    Js.Exn.raiseError("Failed to execute current instruction!")
  }

  instructions
}

// Common
let originalInstructions = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")->parse

let instructionWithNopJmps = originalInstructions->findInstructionWithNopJmp

// Part 1
let finalStateP1 = originalInstructions->operate(initialState)
finalStateP1.currentValue->Js.log

// Part 2
let replaceIndex = instructionWithNopJmps->Belt.Array.map(v => v.index)

let filterLastIndexError = finalStateP2 =>
  finalStateP2->Belt.Array.keep(s => s.error == "LastIndex")->Belt.Array.getExn(0)

let finalStateP2 =
  replaceIndex
  ->Belt.Array.map(replaceAt => {
    let instructions = originalInstructions->replace(replaceAt)
    instructions->operate(initialState)
  })
  ->filterLastIndexError

finalStateP2.currentValue->Js.log
