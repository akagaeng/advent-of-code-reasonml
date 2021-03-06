let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("")

type digits = array<int>

type captcha = int

type captchas = array<captcha>

type state = {idx: int, captcha: int}

type gap = int

let initState = {idx: 0, captcha: 0}

let parse = (inputs): digits =>
  inputs->Belt.Array.map(s => s->Belt.Int.fromString->Belt.Option.getExn)

let digits = inputs->parse

let isSame = (digits: digits, idx1: int, idx2: int) => digits[idx1] === digits[idx2]

let getSum = (digits: digits, state: state, gap: int) => {
  let digitsLen = digits->Belt.Array.length

  switch digits->isSame(state.idx, mod(state.idx + gap, digitsLen)) {
  | true => {...state, captcha: state.captcha + digits[state.idx]}
  | false => state
  }
}

let updateSum = (digits: digits, state: state, gap: gap): state => digits->getSum(state, gap)

let rec findMatch = (digits: digits, state: state, gap: gap): state => {
  let digitsIdxMax = digits->Belt.Array.length - 1
  let thisState = digits->updateSum(state, gap)

  switch thisState.idx < digitsIdxMax {
  | true => digits->findMatch({...thisState, idx: state.idx + 1}, gap)
  | false => thisState
  }
}

let sum = (state: state): captcha => state.captcha

let nextDigitGap = 1

let halfwayGap = (digits: digits) => digits->Belt.Array.length / 2

// part 1
digits->findMatch(initState, nextDigitGap)->sum->Js.log

// part 2
digits->findMatch(initState, digits->halfwayGap)->sum->Js.log
