type inputs_t = array<array<string>>

type coord_t = {x: int, y: int}

type seat_t =
  | Floor(coord_t)
  | Empty(coord_t)
  | Occupied(coord_t)

type seats_t = array<seat_t>

let parse = (inputs: inputs_t): seats_t => {
  inputs
  ->Belt.Array.mapWithIndex((x, row) => {
    row->Belt.Array.mapWithIndex((y, positionCode) => {
      let coord = {x: x, y: y}
      switch positionCode {
      | "." => Floor(coord)
      | "L" => Empty(coord)
      | "#" => Occupied(coord)
      | _ => raise(Not_found)
      }
    })
  })
  ->Belt.Array.concatMany
}

let inputs: inputs_t =
  Node.Fs.readFileAsUtf8Sync("./sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(row => row->Js.String2.split(""))

let seats = inputs->parse
seats->Js.log
