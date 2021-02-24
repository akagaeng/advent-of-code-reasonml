type inputs_t = array<array<string>>

type coord_t = {x: int, y: int}

type seat_t =
  | Floor
  | Empty
  | Occupied

type state_t = {coord: coord_t, complete: bool}

type seats_t = array<{coord: coord_t, seat: seat_t}>

let inputs: inputs_t =
  Node.Fs.readFileAsUtf8Sync("./sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(row => row->Js.String2.split(""))

let seatWidth = inputs->Belt.Array.length

let initialState: state_t = {coord: {x: 0, y: 0}, complete: false}

let parse = (inputs: inputs_t): state_t => {
  inputs
  ->Belt.Array.mapWithIndex((x, row) => {
    row->Belt.Array.mapWithIndex((y, positionCode) => {
      let coord = {x: x, y: y}
      switch positionCode {
      | "." => {seat: Floor, coord: coord}
      | "L" => {seat: Empty, coord: coord}
      | "#" => {seat: Occupied, coord: coord}
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

let getValueAt = (seats: seats_t, thisState: state_t) => {
  // seats->Belt.List.getBy(c =>
  //   switch c {
  //   | Floor(v) => Floor(v)
  //   | Empty(v) => Empty(v)
  //   | Occupied(v) => Occupied(v)
  //   }
  // )

  seats[0]
}

let update = (seats, thisState: state_t) => {
  // 상태 업데이트 - seate의 state를 체크
  // let seatWidth = inputs[0]->Belt.Array.length

  // Js.log(seatWidth)

  // switch thisState.coord {

  Js.log(("thisState:", thisState))

  seats->getValueAt(thisState)

  // }
  // seatWidth
  // seats
}

let move = (seats: seats_t, thisState: state_t) => {
  // 현재 상태만 체크 (thisState가 움직일 게 없는지만 확인)
  // switch thisState.complete {
  // | false =>

  Js.log(("thisState:", thisState))
  let nextState = update(seats, thisState)
  nextState
  // | true => thisState
  // }
  // seats
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
