/*
Given a list $arr and a number $num,
repeat each element in the list $num of times.
input: arr = {1, 2, 3}; num = 3
output: {1, 1, 1, 2, 2, 2, 3, 3, 3}
*/
// approach 1
def f(num: Int, arr: List[Int]): List[Int] = for {
  a <- arr
  i <- 1 to num
} yield a

// approach 2
def f(num: Int, arr: List[Int]): List[Int] = arr.flatMap(List.fill(num)(_))
// or def f(num: Int, arr: List[Int]): List[Int] = arr.flatMap(e => List.fill(num)(e))