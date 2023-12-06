package aoc2023

object Day06 extends Day{
  override val dayNumber: String = "06"
  override val title: String = "Wait For It"

  case class Equation(b: Long, c: Long)

  private def extractNumbers(line: String): Iterator[Int] = {
    """(\d+)""".r.findAllMatchIn(line).map(_.group(1).toInt)
  }

  private def getRoots: Equation => Seq[Double] = eq => {
    val disc = Math.sqrt(1.0 * (eq.b * eq.b - 4 * eq.c))
    Seq(-1, 1).map(x => 0.5 * (eq.b + x * disc))
  }

  private def countIntegersBetween: Seq[Double] => Long = bounds => {
    val Seq(lower, upper) = bounds
    Math.floor(upper).toInt - Math.floor(lower).toInt - (if (lower % 1 == 0) 1 else 0)
  }

  override def part1(data: String): Long = {
    val Array(times, distances) = data.split("\n")
      .map(extractNumbers)

    times.zip(distances).map {
      case (t, d) => (getRoots andThen countIntegersBetween)(Equation(t, d))
    }.product
  }

  override def part2(data: String): Long = {
    val Array(time, distance) = data.split("\n")
      .map(extractNumbers)
      .map(_.mkString("").toLong)

    (getRoots andThen countIntegersBetween)(Equation(time, distance))
  }
}
