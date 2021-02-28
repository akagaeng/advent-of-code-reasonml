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

let inputs: inputs_t =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
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
let move = (seats): seats_t => {
  let findPositionFromCoord = (seats: seats_t, coord: coord_t) => {
    seats->Belt.Array.getBy(x => x.coord == coord)
  }

  let makeAdjacentCoords = (thisCoord: coord_t): array<coord_t> => {
    let {x, y} = thisCoord
    [
      {x: x - 1, y: y + 1},
      {x: x + 0, y: y + 1},
      {x: x + 1, y: y + 1},
      {x: x - 1, y: y + 0},
      {x: x + 1, y: y + 0},
      {x: x - 1, y: y - 1},
      {x: x + 0, y: y - 1},
      {x: x + 1, y: y - 1},
    ]
  }

  let getOccupiedCounts = (seats: seats_t, thisSeat: seat_t): int => {
    let adjacentCoords: array<coord_t> = thisSeat.coord->makeAdjacentCoords
    let occupiedCounts = adjacentCoords->Belt.Array.reduce(0, (acc, adjacentCoord) => {
      let thisPosition = seats->findPositionFromCoord(adjacentCoord)
      switch thisPosition {
      | Some(v) =>
        switch v.position {
        | Occupied => acc + 1
        | _ => acc
        }
      | None => acc
      }
    })
    occupiedCounts
  }

  let findNextPosition = (seats: seats_t, thisSeat: seat_t): position_t => {
    let occupiedCount = seats->getOccupiedCounts(thisSeat)
    if thisSeat.position == Empty && occupiedCount == 0 {
      Occupied
    } else if thisSeat.position == Occupied && occupiedCount >= 4 {
      Empty
    } else {
      thisSeat.position
    }
  }

  let changePosition = (seat: seat_t): seat_t => {
    switch seat.position {
    | Empty
    | Occupied => {...seat, position: findNextPosition(seats, seat)}
    | Floor => {...seat, position: Floor}
    }
  }

  seats->Belt.Array.map(seat => seat->changePosition)
}

let countDiff = (seats: seats_t, newSeats: seats_t): int => {
  seats
  ->Belt.Array.keepWithIndex((seat, i) => {
    seat.position !== newSeats[i].position
  })
  ->Belt.Array.length
}

let rec check = (seats: seats_t, index: int) => {
  let newSeats = seats->move
  let countChangedPositions = countDiff(seats, newSeats)

  Js.log(("Iteration", index))
  Js.log(("countChangedPositions:", countChangedPositions))

  switch countChangedPositions > 0 {
  | true => check(newSeats, index + 1)
  | false => newSeats
  }
}

let countOccupied = (seats: seats_t) =>
  seats->Belt.Array.keep(seat => seat.position == Occupied)->Belt.Array.length

let seats = inputs->parse

seats->check(0)->countOccupied->Js.log

/*
# Rules
  > Adjacent seat 체크: 근처 8방향 (대각선 포함!)
1. Empty(L): Occupied 0개 -> Occupied
2. Occupied (#): 4개 이상의 인접 seat가 Occupied인 경우 -> Empty
3. Floor(.): 변화 없음
*/
