object listsUtil {
  /* Get n sets of input.
     Each input starts with a number k, followed by k pairs of numbers
     example:
     2
     3
     1 2
     3 4
     4 7
     2
     11 11
     22 22
   */
  def get_input(): Unit = {
    val n = io.StdIn.readInt()
    (1 to n).foreach {
      i =>
        val k = io.StdIn.readInt()
        val list = (1 to k).map(x => io.StdIn.readLine.split("\\s+")(0)).toList
        // list contains the first elements of the k pair.
    }
  }

  /*
  Given a list $arr and a number $num,
  repeat each element in the list $num of times.
  input: arr = {1, 2, 3}; num = 3
  output: {1, 1, 1, 2, 2, 2, 3, 3, 3}
  */
  // approach 1: use for loop
  def repeater1(num: Int, arr: List[Int]): List[Int] = for {
    a <- arr
    i <- 1 to num
  } yield a

  // approach 2: use flat map
  def repeater2(num: Int, arr: List[Int]): List[Int] = arr.flatMap(List.fill(num)(_))
  // or def repeater(num: Int, arr: List[Int]): List[Int] = arr.flatMap(e => List.fill(num)(e))


  /*
  For a given list with N integers,
  return a new list removing the elements at odd positions.
  input: arr = { 1, 2, 3, 4, 5}
  output: {1, 3, 5}
  */
  // approach 1, use zip with index
  def remove_odd1(arr: List[Int]): List[Int] = {
    arr.view.zipWithIndex.filter {
      _._2 % 2 != 0
    }.map {
      _._1
    }.toList
  }

  // approach 2, use for loop
  def remove_odd2(arr: List[Int]): List[Int] = {
    for ((a, b) <- arr.zipWithIndex if b % 2 == 1) yield a
  }

  /*
  Given a list, get the sum of odd elements from the given list
  input: arr = {2, 3, 4, 9, 8}
  output: 12
  */
  def sum_odd(arr: List[Int]): Int = arr.filter(_ % 2 != 0).sum
  // Note: -3 % 2 == -1, so checking _%2 == 1 won't work
  // or use reduceLeft(_+_) instead of sum

}