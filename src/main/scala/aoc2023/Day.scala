package aoc2023

trait Day {

  val dayNumber: String

  def part1(data: String): String

  def part2(data: String): String

  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource(s"day$dayNumber.txt").mkString
    println(part1(data))
    println(part2(data))
  }

}
