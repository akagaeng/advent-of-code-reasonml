/*
operation: { acc, jmp, nop }
The accumulator starts at 0.
From the sample: Immediately before the program would run an instruction a second time, 
the value in the accumulator is 5.
*/

type op = {
  line: int,
  operation: string, // acc, jmp, nop
  sign: string, // + / -
  value: int,
}

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

let parse = (strs: array<string>) => {
  strs->Belt.Array.map(str => {
    let line = str->Js.String2.split(" ")
    let (operation, signValue) = (line[0], line[1])
    let (sign, value) = splitSignValue(signValue)

    (operation, sign, value)
  })
}

let instructions = Node.Fs.readFileAsUtf8Sync("./sample.txt")->Js.String2.split("\n")->parse->Js.log
