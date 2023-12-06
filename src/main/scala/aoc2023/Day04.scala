package aoc2023

object Day04 extends Day {
  override val dayNumber: String = "04"
  override val title: String = "Scratchcards"

  def getWins(line: String): Set[Int] = {
    val Array(win, all) = line.split(": ").last
      .split("""\|""")
      .map { part =>
        part.trim
          .split(" ")
          .collect { case v if v.nonEmpty => v.trim.toInt }
          .toSet
      }
    all.intersect(win)
  }

  override def part1(data: String): Int = {
    data.linesIterator.map(getWins)
      .collect { case s if s.nonEmpty => Math.pow(2, s.size - 1).toInt }
      .sum
  }

  override def part2(data: String): Int = {
    val lines = data.split("\n")
    val store = Map.empty[Int, Int]

    lines.zipWithIndex.foldLeft(store) { case (acc, (line, ix)) =>
      val winCnt = getWins(line).size
      (0 until winCnt).foldLeft(acc) {
        case (newAcc, i) =>
          val cardId = ix + i + 1
          newAcc + (cardId -> (newAcc.getOrElse(cardId, 0) + acc.getOrElse(ix, 0) + 1))
      }
    }.values.sum + lines.length
  }
}
