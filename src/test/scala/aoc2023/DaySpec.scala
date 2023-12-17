package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class DaySpec extends AnyFunSuite {
  private val sample =
    """
      |
      |""".stripMargin

  private val worker: Day = ???

  test("Part 1 should handle sample input correctly") {
    assert(worker.part1(sample) === 0)
  }

  test("Part 2 should handle sample input correctly") {
    assert(worker.part2(sample) === 0)
  }
}
