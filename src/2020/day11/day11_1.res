type inputs_t = array<array<string>>

type coord_t = {x: int, y: int}

type position_t =
  | Empty
  | Occupied
  | Floor

type seat_t = {
  coord: coord_t,
  position: position_t,
}

type seats_t = array<seat_t>

type state_t = {seat: seat_t, changedPositions: list<coord_t>}

let inputs: inputs_t =
  Node.Fs.readFileAsUtf8Sync("./sample.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(row => row->Js.String2.split(""))

let parse = (inputs: inputs_t): seats_t => {
  inputs
  ->Belt.Array.mapWithIndex((x, row) => {
    row->Belt.Array.mapWithIndex((y, positionCode) => {
      let coord = {x: x, y: y}
      switch positionCode {
      | "L" => {coord: coord, position: Empty}
      | "#" => {coord: coord, position: Occupied}
      | "." => {coord: coord, position: Floor}
      | _ => raise(Not_found)
      }
    })
  })
  ->Belt.Array.concatMany
}

/*

1. Empty(L): 4방향에 Occupied 없으면 -> Occupied
2. Occupied (#): 4개 이상의 인접 seat가 Occupied인 경우 -> Empty
3. Floor(.): 변화 없음

*/
// let move = (seats, thisState: state_t): state_t => {
//   let findPositionFromCoord = (seats: seats_t, coord: coord_t) => {
//     seats->Belt.Array.getBy(x => x.coord == coord)
//   }

//   let makeAdjacentCoords = (thisCoord: coord_t): array<coord_t> => {
//     let {x, y} = thisCoord
//     [
//       {x: x - 1, y: y + 1},
//       {x: x + 0, y: y + 1},
//       {x: x + 1, y: y + 1},
//       {x: x - 1, y: y + 0},
//       {x: x + 1, y: y + 0},
//       {x: x - 1, y: y - 1},
//       {x: x + 0, y: y - 1},
//       {x: x + 1, y: y - 1},
//     ]
//   }

//   let getOccupiedCounts = (seats: seats_t, thisSeat: seat_t): int => {
//     let adjacentCoords: array<coord_t> = thisSeat.coord->makeAdjacentCoords
//     // calculate occupied counts
//     Js.log(("adjacentCoords:", adjacentCoords))
//     let occupiedCounts = adjacentCoords->Belt.Array.reduce(0, (acc, adjacentCoord) => {
//       let thisPosition = seats->findPositionFromCoord(adjacentCoord)
//       // thisPosition
//       Js.log(("thisPosition", thisPosition))
//       switch thisPosition {
//       | Some(v) => {
//           Js.log(("v.position", v.position))

//           switch v.position {
//           | Occupied => {
//               Js.log(("Occupied!!", acc))
//               acc + 1
//             }
//           | _ => acc
//           }
//         }
//       | None => acc
//       }
//       // 1
//     })

//     // 10
//     // occupiedCounts
//     occupiedCounts
//   }

//   let findNextPosition = (seats: seats_t, thisSeat: seat_t): position_t => {
//     let occupiedCount = seats->getOccupiedCounts(thisSeat)
//     Js.log(("occupiedCount:", occupiedCount))
//     if occupiedCount == 0 {
//       Occupied
//     } else if occupiedCount >= 4 {
//       Empty
//     } else {
//       thisSeat.position
//     }
//   }

//   let changePosition = (seat: seat_t): seat_t => {
//     switch seat.position {
//     | Empty
//     | Occupied => {...seat, position: findNextPosition(seats, seat)}
//     | Floor => {...seat, position: Floor} //
//     }
//   }

//   let isChanged = (prevPos: seat_t, thisPos: seat_t): bool => prevPos == thisPos

//   let hasNoChangedPositions = (changedPositions: list<coord_t>): bool =>
//     changedPositions->Belt.List.length < 1

//   let changedPositions = seats->Belt.Array.reduce(list{}, (acc, seat) => {
//     let newSeat = seat->changePosition
//     // Js.log((" seat (old/new)", seat, newSeat))

//     switch isChanged((seat: seat_t), (newSeat: seat_t)) {
//     | false => acc
//     | true => acc->Belt.List.add(seat.coord)
//     }
//   })

//   let isComplete = changedPositions->hasNoChangedPositions == true ? true : false

//   {
//     seat: seat,
//     changedPositions: changedPositions,
//   }
// }

// changedSeat이 개수가 0일 때..
// let isCompleted = (thisState: state_t): bool => 
// thisState.complete == true


let rec check = (seats: seats_t, thisState: state_t) => {
  switch thisState->isCompleted {
  | true => seats
  | false => {
      let nextState = seats->move(thisState)
      // seats->check(nextState)
      seats->check(nextState)
    }
  }
}

let seats = inputs->parse

// let initialState: state_t = {seats[0], changedPositions: list{}}

seats
->Belt.Array.map(v->)
->Js.log
// ->check(initialState)

/*
# Rules
  > Adjacent seat 체크: 근처 8방향 (대각선 포함!)
1. Empty(L): Occupied 0개 -> Occupied
2. Occupied (#): 4개 이상의 인접 seat가 Occupied인 경우 -> Empty
3. Floor(.): 변화 없음
*/
