package aoc2023

object Day01 extends Day {
  override val dayNumber: String = "01"
  override val title: String = "Trebuchet?!"

  override def part1(data: String): Int = {
    data.linesIterator
      .map { line =>
        val digits = line.filter(_.isDigit)
        s"${digits.head}${digits.last}".toInt
      }.sum
  }

  override def part2(data: String): Int = {
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
}
