type inputs_t = array<array<string>>

type coord_t = {x: int, y: int}

type seat_t =
  | Floor(coord_t)
  | Empty(coord_t)
  | Occupied(coord_t)

type state_t = {coord: coord_t, complete: bool}

type seats_t = array<seat_t>

let inputs: inputs_t =
  Node.Fs.readFileAsUtf8Sync("./sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(row => row->Js.String2.split(""))

let seatWidth = inputs->Belt.Array.length

let initialState: state_t = {coord: {x: 0, y: 0}, complete: false}

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

// let nextCoord = (seat: seat_t, thisState: state_t): state_t => {
//   switch (x, y) {

//   }

// }

let update = (thisState: state_t) => {
  // 상태 업데이트 - seate의 state를 체크
  let seatWidth = inputs[0]->Belt.Array.length
  // Js.log(seatWidth)
  seatWidth
}

let move = (seats: seats_t, thisState: state_t) => {
  // 현재 상태만 체크 (thisState가 움직일 게 없는지만 확인)
  // switch thisState.complete {
  // | false =>
  // update(seats, thisState)
  // | true => thisState
  // }
  seats
}

let seats = inputs->parse
seats->move(initialState)->Js.log

/*
# Rules
  > Adjacent seat 체크: 근처 8방향 (대각선 포함!)
1. Empty(L): 4방향에 Occupied 없으면 -> Occupied
2. Occupied (#): 4개 이상의 인접 seat가 Occupied인 경우 -> Empty
3. Floor(.): 변화 없음
*/
