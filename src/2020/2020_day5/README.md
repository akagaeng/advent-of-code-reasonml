# Day5

## Idea

### Part1
* row, col 둘다 가장 큰 값을 찾는 것
  * input을 정렬 문자 정렬
  * FFFFFFF, LLL 가장 작은 값
  * BBBBBBB, RRR 가장 큰 값
  * 2진수로 생각해서 숫자를 계산 F, L -> 0, B, R -> 1로 치환

### Part 2
* 모든 가능한 케이스에서 FFFFFFF, BBBBBBB 제외
* 내 ID +1, 내 ID - 1은 리스트에 존재함
* row가 동일한 것들끼리 그룹을 먼저 만들고, 그것들 중에서 개수가 적은 것을 선택
## Soultions
