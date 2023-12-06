package aoc2023

trait Day {

  val dayNumber: String

  val title: String = ""

  def part1(data: String): AnyVal

  def part2(data: String): AnyVal

  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource(s"day$dayNumber.txt").mkString
    println(s"== Day $dayNumber: $title ==")
    println(s"Part 1: ${part1(data).toString}")
    println(s"Part 2: ${part2(data).toString}")
  }
}
