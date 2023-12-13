package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day11Spec extends AnyFunSuite {
  private val sample =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....
      |""".stripMargin

   private val worker = Day11

  test("Part 1 should handle sample input correctly") {
    assert(worker.part1(sample) === 374)
  }

  test("Part 2 should handle sample input correctly") {
    assert(worker.findDistances(sample.split("\n"), 99).sum === 8410)
  }
}
