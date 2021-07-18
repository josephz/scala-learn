import scala.io._

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

  /* Compute the perimeter of a polygon in 2-D
  input: The first line contains an integer, N, denoting the number of points.
  The N subsequent lines each contain 2 space-separated integers denoting the respective x and y coordinates of a point.
  e.g. a square whose perimeter is 4
  input: 4
        0 0
        0 1
        1 1
        1 0
  output: 4
   */
  def distance(p1: (Int, Int), p2: (Int, Int)): Double =
  // compute the distance of two points on the plane
    math.sqrt(math.pow(p1._1 - p2._1, 2) + math.pow(p1._2 - p2._2, 2))

  def perimeter() {
    val lines = Source.stdin.getLines()

    // parse the input
    val nPoints = lines.next.toInt
    val points = lines.take(nPoints).toList.map(s => s.split(" ")).map {
      case Array(e1, e2) => (e1.toInt, e2.toInt)
    }

    // form the polygonal line, by appending the head point to the end of the list
    val polygLine = points :+ points.head

    val perimeter = polygLine.tail.foldLeft((0.0, polygLine.head)) {
      case (acc, p) => (acc._1 + distance(acc._2, p), p)
    }._1

    println(perimeter)
  }
}