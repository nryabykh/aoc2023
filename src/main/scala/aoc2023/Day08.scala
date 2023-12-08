package aoc2023

import scala.annotation.tailrec

case class Node(name: String, left: String, right: String)

case class Game(turns: String, map: Map[String, Node]) {

  @tailrec
  final def makeStep(currentPos: String, currentStep: Int, checkFn: String => Boolean): Int = {
    currentPos match {
      case x if checkFn(x) => currentStep
      case _ =>
        val next = if (turns(currentStep % turns.length) == 'L') map(currentPos).left else map(currentPos).right
        makeStep(next, currentStep + 1, checkFn)
    }
  }
}

object Day08 extends Day {
  override val dayNumber: String = "08"
  override val title: String = "Haunted Wasteland"
  override val link: String = "https://adventofcode.com/2023/day/8"

  @tailrec
  private def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
  private def lcm: (Long, Long) => Long = (a, b) => (a * b) / gcd(a, b)

  private def parse(data: String): Game = {
    val input = data.split("\n").filter(_.nonEmpty)
    val turns = input.head.trim
    val map = input.drop(1).flatMap { line =>
      """(\w+) = \((\w+), (\w+)\)""".r.findAllMatchIn(line).map(m =>
        (m.group(1), Node(m.group(1), m.group(2), m.group(3))))
    }.toMap

    Game(turns, map)
  }

  override def part1(data: String): AnyVal = {
    parse(data).makeStep("AAA", 0, _ == "ZZZ")
  }

  override def part2(data: String): AnyVal = {
    val game = parse(data)

    game.map.keySet
      .filter(_.endsWith("A"))
      .map(s => game.makeStep(s, 0, _.endsWith("Z")).toLong)
      .reduce(lcm)
  }
}
