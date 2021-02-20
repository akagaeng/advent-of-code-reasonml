type number_t = int
type numbers_t = array<number_t>
type option_t = {
  currentIndex: int,
  length: int,
}

let inputs: numbers_t =
  Node.Fs.readFileAsUtf8Sync("./sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(s => s->Belt.Int.fromString->Belt.Option.getExn)

let hasSum = (inputs: numbers_t, value: int): int => value

let validate = (inputs: numbers_t, option: option_t): numbers_t => {
  Js.log((option.currentIndex, option.length))

  let thisPreamble = inputs->Belt.Array.slice(~offset=option.currentIndex, ~len=option.length)
  let thisValue = inputs[option.length + option.currentIndex]
  Js.log(("thisPreamble:", thisPreamble, "thisValue:", thisValue))

  inputs
}

let initialStartSample = 5
let initialStart = 25

let initialOption = {currentIndex: 0, length: initialStartSample}

inputs->validate(initialOption)->Js.log
