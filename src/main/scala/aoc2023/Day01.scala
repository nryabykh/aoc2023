package aoc2023

object Day01 {

  def part1(data: String): Int = {
    data.linesIterator
      .map { line =>
        val digits = line.filter(_.isDigit)
        s"${digits.head}${digits.last}".toInt
      }.sum
  }

  def part2(data: String): Int = {
    val replacers =
      Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zipWithIndex ++
        (1 to 9).map(_.toString).zipWithIndex

    data.linesIterator
      .map { line =>
        val firstIds = replacers.map {
          case (word, digit) => (line.indexOf(word), digit + 1)
        }
          .filterNot { case (ix, _) => ix == -1 }
          .sorted

        val lastIds = replacers.map {
          case (word, digit) => (line.lastIndexOf(word), digit + 1)
        }
          .filterNot { case (ix, _) => ix == -1 }
          .sorted

        s"${firstIds.head._2}${lastIds.last._2}".toInt
      }.sum
  }

  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("day01.txt").mkString
    println(part1(data))
    println(part2(data))
  }

}
