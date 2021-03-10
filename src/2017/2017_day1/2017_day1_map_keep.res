let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("")

type digits = array<int>

type captcha = int

type captchas = array<captcha>

type gap = int

let parse = (inputs): digits =>
  inputs->Belt.Array.map(s => s->Belt.Int.fromString->Belt.Option.getExn)

let digits = inputs->parse

let isSame = (digits: digits, idx1: int, idx2: int) => digits[idx1] === digits[idx2]

let findMatched = (digits: digits, gap: int) => {
  let digitsLen = digits->Belt.Array.length
  digits->Belt.Array.keepWithIndex((_, idx) => {
    digits->isSame(idx, mod(idx + gap, digitsLen)) === true
  })
}

let nextDigitGap: gap = 1

let getHalfwayGap = (digits: digits): gap => digits->Belt.Array.length / 2

let calculateCaptcha = (digits: digits): captcha => digits->Belt.Array.reduce(0, (a, v) => a + v)

// part 1
digits->findMatched(nextDigitGap)->calculateCaptcha->Js.log

// part 2
digits->findMatched(digits->getHalfwayGap)->calculateCaptcha->Js.log
