type frequency = int
type frequencies = array<frequency>
type state = {idx: int, frequencies: frequencies, acc: int, reached: list<int>}

let input = Node.Fs.readFileAsUtf8Sync("./input.txt")

let parse = (input): frequencies =>
  input->Js.String2.split("\n")->Belt.Array.map(s => s->int_of_string)

let frequencies = input->parse

let initState = {idx: 0, frequencies: frequencies, acc: 0, reached: list{}}

let next = (normalize, state): state => {
  let thisIdx = state.idx
  let frequenciesLen = state.frequencies->Belt.Array.length
  let thisAcc = state.acc + state.frequencies[thisIdx]

  {
    ...state,
    idx: normalize(thisIdx + 1, frequenciesLen),
    acc: thisAcc,
    reached: list{thisAcc, ...state.reached},
  }
}

let loop = (terminateCondition, normalize, state): state => {
  let rec run = state => {
    switch state->terminateCondition {
    | false => run(normalize->next(state))
    | true => state
    }
  }
  run(state)
}

let getAcc = state => state.acc

let getReachedFrequency = state => state.acc + state.frequencies[state.idx]

// part 1
// - short solution: using reduce
// frequencies->Belt.Array.reduce(0, (acc, item) => acc + item)->Js.log

// - solution: using rec function
let p1Condition = (state): bool => state.idx >= state.frequencies->Belt.Array.length

let p1Normalize = (a: int, _: int): int => a

let p1Loop = initState => loop(p1Condition, p1Normalize, initState)

p1Loop(initState)->getAcc->Js.log

// part 2
let p2Condition = (state): bool =>
  state.reached->Belt.List.some(v => v === state.acc + state.frequencies[state.idx])

let p2Normalize = (a: int, b: int): int => mod(a, b)

let p2Loop = initState => loop(p2Condition, p2Normalize, initState)

p2Loop(initState)->getReachedFrequency->Js.log
