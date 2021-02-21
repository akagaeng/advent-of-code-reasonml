type seat_t = array<string>
type seats_t = array<seat_t>

let seats =
  Node.Fs.readFileAsUtf8Sync("./sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(row => row->Js.String2.split(""))

seats->Js.log
