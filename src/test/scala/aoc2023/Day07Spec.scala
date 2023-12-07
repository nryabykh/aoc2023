package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day07Spec extends AnyFunSuite{
  private final val sample =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483
      |""".stripMargin

  test("Part 1 should handle sample input correctly") {
    assert(Day07.part1(sample) === 6440)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day07.part2(sample) === 5905)
  }

}
