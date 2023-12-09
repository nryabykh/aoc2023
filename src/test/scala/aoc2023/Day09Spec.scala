package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day09Spec extends AnyFunSuite {
  private val sample =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45
      |""".stripMargin

  private val worker = Day09

  test("Part 1 should handle sample input correctly") {
    assert(worker.part1(sample) === 114)
  }

  test("Part 2 should handle sample input correctle") {
    assert(worker.part2(sample) === 2)
  }
}
