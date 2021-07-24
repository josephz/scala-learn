import scala.annotation.tailrec

object mathsUtil {
  /*
  e.g., to read in line as Integer
  stdin = scala.io.StdIn.readLine.trim.toInt
   */

  /* Evaluating Euler constants to the 10th term:
    e^x = 1 + x + x^2/2! + x^3/3! + ...
   */
  def term(x: Double, acc: Double, t: Double): Double = {
    if (t > 10.0) 0.0
    else acc + term(x, acc * (x / t), t + 1.0)
  }

  def euler_pow(x: Double) = term(x, 1.0, 1.0)

  /* Compute the perimeter and area of a polygon in 2-D
  input: The first line contains an integer, N, denoting the number of points.
  The N subsequent lines each contain 2 space-separated integers denoting the respective x and y coordinates of a point.
  e.g. a square whose perimeter is 4 and area is 1
  input: 4
        0 0
        0 1
        1 1
        1 0
  perimeter output: 4
  area output: 1
   */
  // compute the distance of two points on the plane
  def distance(p1: (Int, Int), p2: (Int, Int)): Double =
    math.sqrt(math.pow(p1._1 - p2._1, 2) + math.pow(p1._2 - p2._2, 2))

  // compute the term in the Green's Theorem
  def area_term(p1: (Int, Int), p2: (Int, Int)): Double =
    0.5 * (p2._1 + p1._1) * (p2._2 - p1._2)

  def perimeter_area() {
    val lines = io.Source.stdin.getLines()

    // parse the input
    val nPoints = lines.next.toInt
    val points = lines.take(nPoints).toList.map(s => s.split(" ")).map {
      case Array(e1, e2) => (e1.toInt, e2.toInt)
    }

    // form the polygonal line, by appending the head point to the end of the list
    val polygLine = points :+ points.head

    // starting from the 2nd left of the list, accumulate the distance
    val perimeter = polygLine.tail.foldLeft((0.0, polygLine.head)) {
      case (acc, p) => (acc._1 + distance(acc._2, p), p)
    }._1

    println(perimeter)

    // accumulate the area terms
    val area = polygLine.tail.foldLeft((0.0, polygLine.head)) {
      case (acc, p) => (acc._1 + area_term(acc._2, p), p)
    }._1

    println(area)
  }

  /* Compute GCD of two integers
   */
  @tailrec
  def gcd(x: Int, y: Int): Int = if (x == 0 || y == 0) x + y else gcd(y, x % y)

  /* Compute Fibonacci numbers
   */
  def fibonacci(x: Int): Int = if (x == 1 || x == 2) x - 1 else fibonacci(x - 1) + fibonacci(x - 2)

  /* Print Pascal's Triangle
   1- the brute-force way, which calculates the factorials
   2- the dynamic programming way, which builds items in the row one by one
   3- the dynamic programming way, which builds row n+1 from row n
   val digit = io.Source.stdin.getLines.take(1).map(_.trim.toInt).next()
   */
  def pascal_1(digit: Int): Unit = {
    def factorial(num: Int) = (1 to num).product

    (0 until digit).foreach(n => {
      (0 to n).foreach(r => {
        print(factorial(n) / (factorial(r) * factorial(n - r)) + " ")
      })
      println()
    })
  }

  def pascal_2(digit: Int): Unit = {
    def nthRow(n: Int): List[Int] = {
      val row = List(1)
      (0 until n).foldLeft[List[Int]](row)((acc, r) => acc.head * (n - r) / (r + 1) :: acc)
    }

    (0 until digit).foreach(
      i => {
        nthRow(i).foreach(x => print(x + " "))
        println()
      }
    )
  }

  def pascal_3(digit: Int): Unit = {
    var rolling_row = List[Int](1)

    (0 until digit).foreach(n => {
      // print the current row
      for (item <- rolling_row) print(item + " ")
      println()
      // build the next row
      if (n != 0) {
        rolling_row = 1 :: rolling_row.sliding(2).map(_.sum).toList
        rolling_row = rolling_row :+ 1
      }
    })
  }
}