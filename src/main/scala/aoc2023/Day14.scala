package aoc2023

object Day14 extends Day {
  override val dayNumber: String = "14"


  def transpose(lines: Seq[String]): Seq[String] = {
    (0 until lines.head.length).map { x =>
      lines.map(line => line(x)).mkString("")
    }
  }

  override def part1(data: String): AnyVal = {
    val lines = transpose(data.split("\n"))
    lines.map { line =>
      val (rollsPos, _) = line.zipWithIndex.foldLeft((Set.empty[Int], -1)) {
        case ((rolls, lim), (ch, ix)) =>
          ch match {
            case '#' => (rolls, ix)
            case '.' => (rolls, lim)
            case 'O' => (rolls + (lim + 1), lim + 1)
          }
      }
      rollsPos.map(x => line.length - x).sum
    }.sum
  }

  override def part2(data: String): AnyVal = {
//    val lines = data.split("\n")
//    val rocksRows: Map[Int, Set[Int]] = lines.zipWithIndex.map { case(line, y) =>
//      (y, line.zipWithIndex.collect { case(ch, x) if ch == '#' => x }.toSet)
//    }.toMap
//
//    val rocksCols: Map[Int, Set[Int]] = (0 until lines.head.length).map { x =>
//      (x, lines.indices.filter(y => lines(x)(y) == '#').toSet)
//    }.toMap
    0



  }
}
