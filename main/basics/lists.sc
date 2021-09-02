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
      // or take the pair: pair = io.StdIn.readLine().trim().split(" ").map(x=>x.toInt).toList
      // then the list is a pair: (pair.head, pair.reverse.head)
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

  /*
  Given two lists, zip the content and flatten the output
  input: List(a, b, c, d, e);  List(p, q, r, s, t)
  output: apbqcrdset
   */
  def zip_lists(p: List[Int], q: List[Int]): Unit = {
    val z = p.lazyZip(q).flatMap(List(_, _))
    z.foreach(print)
  }

  /*
  Filter elements which are repeated at least K times
  input: K=2, array = {4, 5, 2, 5, 4, 3, 1, 3, 4}
  output: 4 5 3  // array
  input: K=4, array = {4, 5, 2, 5, 4, 3, 1, 3, 4}
  output: nil   // not found
   */
  def filterElements(k: Int, arr: Array[Int]): Array[Int] = {
    val occurrences = arr.groupMapReduce(identity)(_ => 1)(_ + _)
    //groups list elements (group part of groupMapReduce)
    //maps each grouped value occurrence to 1 (map part of groupMapReduce)
    //reduces values within a group of values (_ + _) by summing them (reduce part of groupMapReduce).
    // before v2.13    val occurrences = arr.groupBy(identity).mapValues(_.length)
    arr.distinct.filter(occurrences(_) >= k)
  }

  /*
  Count the occurrences in the list of generic items
  e.g.
  input: List(13, 16, 12, 13, 19, 12, 13)
  output: Map(13 -> 3, 16 -> 1, 12 -> 2, 19 -> 1)
   */
  def countMap[T](l: List[T]): Map[T, Int] = {
    val m = Map.empty[T, Int].withDefaultValue(0)
    l.foldLeft(m)((m, e) => m + (e -> (m(e) + 1)))
  }

  /*
  List out the missing items from List B in List A
  input: List A and List B
  output: distinct list of items that in List B but not in list A
  Note: the count of the items matters
  e.g.
  input: A: List(12, 13, 16, 12)
         B: List(13, 16, 12, 13, 19, 12, 13)
  output: List(13, 19)
   */
  def missingValues[T](A: List[T], B: List[T]): List[T] = {
    val (mapA, mapB) = (countMap(A), countMap(B))
    (mapA.keySet | mapB.keySet)
      .filterNot(n => mapA(n) == mapB(n))
      .toList
  }

  /*
  Given a list of integers, count the minimum number of integers required, so that the sum reach a target value.
  e.g.
  input: target = 10
         A: List(1, 2, 3, 4, 5, 6, 7)
  output: 2
  // it requires at least two number in list A to reach target 10. for example, 5+7 > 10
  // if the target cannot be reached, return -1
   */
  def numForSum(target: Long, A: List[Long]): Int = {
    val max = A.sum
    if (target > max) return -1

    var count = 0
    var current: Long = 0
    A.sorted(Ordering.Long.reverse).iterator.takeWhile(_ => target > current).foreach(k => {
      current = current + k
      count = count + 1
    })
    count
  }
}