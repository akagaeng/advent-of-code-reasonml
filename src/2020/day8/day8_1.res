/*
operation: { acc, jmp, nop }
The accumulator starts at 0.
From the sample: Immediately before the program would run an instruction a second time, 
the value in the accumulator is 5.
*/

type line = {
  index: int,
  operation: string, // acc, jmp, nop
  sign: string, // + / -
  value: int,
  count: int,
}

type operation = array<line>

let splitSignValue = signValue => {
  let re = %re("/(\+|\-)([0-9]+)/")
  let result = Js.Re.exec_(re, signValue)

  switch result {
  | Some(r) => (
      Js.Nullable.toOption(Js.Re.captures(r)[1]), // sign
      Js.Nullable.toOption(Js.Re.captures(r)[2]), // value
    )
  | None => (None, None)
  }
}

let parse = (strs: array<string>): operation => {
  strs->Belt.Array.mapWithIndex((i, str) => {
    let line = str->Js.String2.split(" ")
    let (operation, signValue) = (line[0], line[1])
    let (sign, value) = splitSignValue(signValue)

    {
      index: i,
      operation: operation,
      sign: sign->Belt.Option.getExn,
      value: value->Belt.Option.getExn->Belt.Int.fromString->Belt.Option.getExn,
      count: 0,
    }
  })
}

let instructions = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n")->parse->Js.log
