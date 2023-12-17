package aoc2023

import scala.collection.mutable

object Day15 extends Day {
  override val dayNumber: String = "15"
  override val title: String = "Lens Library"
  override val link: String = "https://adventofcode.com/2023/day/15"


  private def getHash(line: String): Int = {
    line.toCharArray.foldLeft(0L) {
      case (acc, ch) => (acc + ch.toInt.toLong) * 17 % 256
    }.toInt
  }

  override def part1(data: String): AnyVal = {
    data.split("\n").head.split(",").map(getHash).sum
  }

  type Box = mutable.LinkedHashMap[String, Int]

  override def part2(data: String): AnyVal = {
    val boxes = Map.empty[Int, mutable.LinkedHashMap[String, Int]]
    val resultBoxes = data.split("\n").head.split(",").foldLeft(boxes) { case (acc, item) =>
      val label = """[a-z]+""".r.findFirstIn(item).get
      val boxNumber = getHash(label)
      val currentBox: Box = acc.getOrElse(boxNumber, mutable.LinkedHashMap.empty[String, Int])

      val action = item.drop(label.length)(0)
      val newBox: Box = action match {
        case '-' => if (currentBox.contains(label)) currentBox -= label else currentBox
        case '=' =>
          val value = """\d+""".r.findFirstIn(item).get.toInt
          currentBox + (label -> value)
      }
      acc + (boxNumber -> newBox)
    }

    resultBoxes.flatMap { case(boxNumber, items) =>
      items.zipWithIndex.map { case((_, v), ix) => (boxNumber + 1) * (ix + 1) * v }
    }.sum
  }
}
