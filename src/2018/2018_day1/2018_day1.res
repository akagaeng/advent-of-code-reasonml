type frequency = int
type frequencies = array<frequency>

let input = Node.Fs.readFileAsUtf8Sync("./input.txt")
let parse = (input): frequencies =>
  input->Js.String2.split("\n")->Belt.Array.map(s => s->int_of_string)
let frequencies = input->parse

// part 1
frequencies->Belt.Array.reduce(0, (acc, item) => acc + item)->Js.log

// part 2
