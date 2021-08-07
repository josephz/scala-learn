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
  Read in even-numbered char strings, swap adjacent char in each string and print
  input: abcdpqrs
  output: badcqpsr
   */
  def swap_char() = {
    io.Source.stdin.getLines.toList
      .map(_.grouped(2).map(_.reverse).mkString(""))
      .map(println)
  }

  /*
  Count repeated chars in a string and output a compressed version
  input: abcaaabbb
  output: abca3b3
  input: aaabaaaaccaaaaba
  output: a3ba4c2a4ba
  Note: counting of 1 is not displayed
   */
  // approach 1: use regex to parse the string
  def compress_repeated_regex(input: String): String = "(\\w)\\1+".r.replaceAllIn(input, m => "$1" + m.toString.length)

  // approach 2: use recursion, count similar chars bit-by-bit
  def compress_repeated_recur(input: String): String = {
    def process(l: List[Char], acc: String = ""): String = {
      l match {
        case Nil => acc
        case h :: _ =>
          val tw = l.takeWhile(_ == h)
          acc + process(l.drop(tw.length),
            if(tw.length > 1) h + tw.length.toString else h.toString)
      }
    }
    process(input.toList)
  }
}