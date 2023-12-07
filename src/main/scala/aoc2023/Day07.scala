package aoc2023

object Day07 extends Day {
  override val dayNumber: String = "07"
  override val title: String = "Camel Cards"
  override val link: String = "https://adventofcode.com/2023/day/7"

  private def parse: String => Seq[(String, Int)] = input => {
    input.split("\n").map { line =>
      val Array(hand, bid) = line.split(" ")
      (hand, bid.toInt)
    }
  }

  private def solve: Seq[Hand] => Int = hands => {
    hands
      .sortBy(h => (h.counted, h.lineForSort))
      .zipWithIndex
      .map { case (hand, ix) =>
        hand.bid * (ix + 1)
      }.sum
  }

  override def part1(data: String): AnyVal = {
    def getInstance: Seq[(String, Int)] => Seq[Hand] =
      lines => lines.map { case(hand, bid) => SimpleHand(hand, bid) }

    (parse andThen getInstance andThen solve)(data)
  }

  override def part2(data: String): AnyVal =  {
    def getInstance: Seq[(String, Int)] => Seq[Hand] =
      lines => lines.map { case (hand, bid) => HandWithJoker(hand, bid) }

    (parse andThen getInstance andThen solve)(data)
  }
}

trait Hand {
  val replacements: Seq[(Char, Char)]
  val counted: String
  val line: String
  val bid: Int

  def lineForSort: String = replacements.foldLeft(line) {
    case (acc, (from, to)) => acc.replace(from, to)
  }
}

case class SimpleHand(line: String, bid: Int) extends Hand {
  override val replacements: Seq[(Char, Char)] = Seq(('T', 'C'), ('K', 'X'), ('A', 'Z'))

  override val counted: String = line.groupBy(_.toChar)
    .map { case(_, v) => v.length }
    .mkString("").sorted.reverse
}

case class HandWithJoker(line: String, bid: Int) extends Hand {
  override val replacements: Seq[(Char, Char)] = Seq(('T', 'C'), ('K', 'X'), ('A', 'Z'), ('J', '1'))

  override val counted: String = {
    val countMap = line.groupBy(_.toChar).map { case (k, v) => k -> v.length }
    val jokerCnt = countMap.getOrElse('J', 0)
    countMap - 'J' match {
      case m if m.isEmpty => "5"
      case mapWithoutJoker =>
        val (maxItem, maxValue) = mapWithoutJoker.maxBy { case (_, cnt) => cnt }
        (mapWithoutJoker + (maxItem -> (maxValue + jokerCnt)))
          .values
          .mkString("").sorted.reverse
    }
  }
}
