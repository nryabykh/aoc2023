package aoc2023

object Day02 extends Day {
  override val dayNumber: String = "02"
  override val title: String = "Cube Conundrum"

  def getResults(line: String): Map[String, Int] = {
    val regex = """(\d+)\s(\w+)(,|;|$)""".r
    val init = Map("blue" -> 0, "red" -> 0, "green" -> 0)

    val games = line.split(":").last.trim

    regex.findAllMatchIn(games).foldLeft(init) {
      case (acc, m) => acc + (m.group(2) -> Math.max(m.group(1).toInt, acc(m.group(2))))
    }
  }

  override def part1(data: String): Int = {
    data.linesWithSeparators.map { line =>
      val gameId = line.split(":").head.split(" ").last.toInt
      val maxs = Map("blue" -> 14, "red" -> 12, "green" -> 13)

      val result = getResults(line)
      if (Seq("blue", "red", "green").map(c => result(c) <= maxs(c)).reduce(_ && _)) gameId else 0
    }.sum
  }

  override def part2(data: String): Int = {
    data.linesWithSeparators.map(getResults(_).values.product).sum
  }
}
