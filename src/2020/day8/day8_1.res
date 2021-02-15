let inputs = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

/*
operation: { acc, jmp, nop }
The accumulator starts at 0.
From the sample: Immediately before the program would run an instruction a second time, 
the value in the accumulator is 5.
*/

type op = {
  line: int,
  operation: string,  // acc, jmp, nop
  sign: string, // + / -
  value: int,
}

inputs->Js.log
