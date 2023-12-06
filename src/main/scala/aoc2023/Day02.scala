package aoc2023

object Day02 {

  def getResults(line: String): Map[String, Int] = {
    val regex = """(\d+)\s(\w+)(,|;|$)""".r
    val init = Map("blue" -> 0, "red" -> 0, "green" -> 0)

    val games = line.split(":").last.trim

    regex.findAllMatchIn(games).foldLeft(init) {
      case (acc, m) => acc + (m.group(2) -> Math.max(m.group(1).toInt, acc(m.group(2))))
    }
  }

  def part1(data: String): Int = {
    data.linesWithSeparators.map { line =>
      val gameId = line.split(":").head.split(" ").last.toInt
      val maxs = Map("blue" -> 14, "red" -> 12, "green" -> 13)

      val result = getResults(line)
      if (Seq("blue", "red", "green").map(c => result(c) <= maxs(c)).reduce(_ && _)) gameId else 0
    }.sum
  }

  def part2(data: String): Int = {
    data.linesWithSeparators.map(getResults(_).values.product).sum
  }

  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("day02.txt").mkString
    println(part1(data))
    println(part2(data))
  }

}
