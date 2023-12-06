package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day06Spec extends AnyFunSuite{
  private final val sample =
    """Time:      7  15   30
      |Distance:  9  40  200
      |""".stripMargin

  test("Part 1 should handle sample input correctly") {
    assert(Day06.part1(sample) === "288")
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day06.part2(sample) === "71503")
  }
}
