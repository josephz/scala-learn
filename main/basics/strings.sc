object stringsUtil {
  /*
   Read in even-numbered char strings, swap adjacent char in each string and print
   input: abcdpqrs
   output: badcqpsr
    */
  def swap_char(): Unit = {
    io.Source.stdin.getLines.toList
      .map(_.grouped(2).map(_.reverse).mkString(""))
      .foreach(println)
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
            if (tw.length > 1) h + tw.length.toString else h.toString)
      }
    }

    process(input.toList)
  }

  /*
  Find the common prefix of two strings, and their remainders
  input: s1 = abcdefpr; s2 = abcpqr
  output: (abc, defpr, pqr)
   */
  def prefix_remainder(s1: String, s2: String): (String, String, String) = {
    val prefix = s1.zip(s2).takeWhile { case (xc, yc) => xc == yc }.map(_._1).mkString("")
    val p_length = prefix.length
    (prefix, s1.drop(p_length), s2.drop(p_length))
  }

  /*
  De-duplicate characters in a string
  Input: accabb
  Output: acb
  Note: the output of each approach is different in their letter orders
   */
  // approach 1: use foldLeft -- preserves occurrence's order
  def string_reduction(s: String): String = s.toList.foldLeft("")((b, a) => if (b.contains(a)) b else b + a)

  // approach 2: use set -- order may not be preserved
  def string_reduction2(s: String): String = s.toSet.mkString("")

  // approach 3: use distinct -- preserves order
  def string_reduction3(s: String): String = s.toList.distinct.mkString("")
}