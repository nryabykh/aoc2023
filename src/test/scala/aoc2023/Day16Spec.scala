package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day16Spec extends AnyFunSuite {
  private val sample =
    """.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....
      |""".stripMargin

  private val worker: Day = Day16

  test("Part 1 should handle sample input correctly") {
    assert(worker.part1(sample) === 46)
  }

  test("Part 2 should handle sample input correctly") {
    assert(worker.part2(sample) === 51)
  }
}
