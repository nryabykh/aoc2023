package aoc2023

object Day13 extends Day {
  override val dayNumber: String = "13"
  override val title: String = "Point of Incidence"
  override val link: String = "https://adventofcode.com/2023/day/13"


  def countDiffs(line: String, x: Int): Int = {
    val left = line.take(x)
    val right = line.drop(x)
    val deltaLength = left.length - right.length
    val adjRight = if (deltaLength < 0) right.dropRight(-deltaLength) else right
    val adjLeft = if (deltaLength > 0) left.drop(deltaLength) else left
    adjLeft.zipWithIndex.count { case(ch, ix) => ch != adjRight.reverse(ix) }
  }

  def transpose(lines: Seq[String]): Seq[String] = {
    (0 until lines.head.length).map { x =>
      lines.map(line => line(x)).mkString("")
    }
  }

  def findReflection(lines: Seq[String], numberOfSmudges: Int = 0): Int = {
    (1 until lines.head.length).map { ix =>
      lines.map(line => countDiffs(line, ix)).sum
    }.indexOf(numberOfSmudges) + 1
  }

  override def part1(data: String): AnyVal = {
    val patterns = data.split("\n\n")
    patterns.map { pat =>
      val hor = pat.split("\n")
      val vert = transpose(hor)
      100 * findReflection(vert) + findReflection(hor)
    }.sum
  }

  override def part2(data: String): AnyVal = {
    val patterns = data.split("\n\n")
    patterns.map { pat =>
      val hor = pat.split("\n")
      val vert = transpose(hor)
      100 * findReflection(vert, 1) + findReflection(hor, 1)
    }.sum
  }
}
