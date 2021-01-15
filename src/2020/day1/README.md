# Soultion

## Versions

* [Initial version](#initial-version)
  * Using for loop
* Refactor
  * [Input with pipe](#input-with-pipe)
  * [Input with pipe more](#input-with-pipe-more)
  * [Using index of array](#using-index-of-array)
  * [Prepared coordinates](#prepared-coordinates)
## Initial version

```reasonml
let input = Node.Fs.readFileAsUtf8Sync("./input.txt")
let inputsStr = Js.String.split("\n", input)
let inputsOptionInt = Belt.Array.map(inputsStr, v => Belt.Int.fromString(v))
let inputsInt = Belt.Array.map(inputsOptionInt, v => Belt.Option.getExn(v))

let idxMax = Belt.Array.length(inputsInt) - 1

// Part One
for i in 0 to idxMax {
  for j in i + 1 to idxMax - i {
    let x = inputsInt[i]
    let y = inputsInt[j]
    let sum = x + y

    if sum === 2020 {
      let mul = x * y
      Js.log("Part One: " ++ Belt.Int.toString(mul))
    }
  }
}

// Part Two
for i in 0 to idxMax {
  for j in i + 1 to idxMax - i {
    for k in j + 1 to idxMax - j {
      let x = inputsInt[i]
      let y = inputsInt[j]
      let z = inputsInt[k]
      let sum = x + y + z

      if sum === 2020 {
        let mul = x * y * z
        Js.log("Part Two: " ++ Belt.Int.toString(mul))
      }
    }
  }
}

```

## Input with pipe

```reasonml
let inputStr = Node.Fs.readFileAsUtf8Sync("./input.txt")
let inputs =
  Js.String.split("\n", inputStr)
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))
```

## Input with pipe more
```reasonml
let inputs =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(i => Belt.Int.fromString(i))
  ->Belt.Array.map(o => Belt.Option.getExn(o))
```

## 변수 개수를 하나 줄이기
* 최종값 - 현재값 으로 검색 문제로 변환
* for loop로 풀이
* map을 이용한 풀이

```reasonml
// target을 찾는 것으로 변경
for i in 0 to idxMax {
  for j in i + 1 to idxMax - i {
    let target = 2020 - inputs[i]
    let thisValue = inputs[j]

    if thisValue === target {
      let mul = thisValue * target
      Js.log("Part One: " ++ Belt.Int.toString(mul))
    }
  }
}
```

## Using index of array
* index의 목록을 만든 후에, 이를 map으로 순회하면서 값을 찾음
* for loop -> map

## Prepared Coordinates

### 전략
1. 확인할 좌표 목록의 배열(json) 구하기 (x, y) [{x: x1, y: y1}, {x: x2, y: y2}, ...]
    * 이 때, i=0일 때 j = 1, 2, 3, ..., i=1일 때는 j=2, 3, 4, ... 여야 함 
    * x, y 두개인 경우에는 zip을 이용한 풀이가 가능할 듯 (https://rescript-lang.org/docs/manual/latest/api/belt/array#zip)
    * 3개인 경우에는 array of tuple (또는 type?) 을 이용해야할 것으로 보임
2. 좌표 목록 map 으로 돌면서 그 좌표의 값들의 합이 2002인지 확인
