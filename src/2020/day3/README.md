# Day3

## Idea
tree의 수를 구하는 것이므로 계산의 편의를 위해 다음과 같이 지정하여 tree를 더해주면 될 것
* tree = 1
* open square = 0

## Soultions
* 2차원 array를 이용하여 풀이
* array of dict(k-v)를 이용 
  * 1,3 이동이면 array의 idx +=3, dict key +=3 의 value를 구하면 됨