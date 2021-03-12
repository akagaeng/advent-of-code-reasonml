type frequency = int
type frequencies = array<frequency>
type state = {idx: int, reached: list<int>}

let initState = {idx: 0, reached: list{}}
let input = Node.Fs.readFileAsUtf8Sync("./input.txt")
let parse = (input): frequencies =>
  input->Js.String2.split("\n")->Belt.Array.map(s => s->int_of_string)
let frequencies = input->parse

let next = (condition, frequencies, state): state => {
  {idx: state.idx + 1, reached: list{frequencies[state.idx], ...state.reached}}
}

let rec loop = (condition, frequencies, state): state => {
  switch condition {
  | false => next(condition, frequencies, state)
  | true => state
  }
}

// part 1
// frequencies->Belt.Array.reduce(0, (acc, item) => acc + item)->Js.log
let condition = true

loop(condition, frequencies, initState)->Js.log

// part 2
