package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day14Spec extends AnyFunSuite {
  private val sample =
    """O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....
      |""".stripMargin

  private val worker = Day14

  test("Part 1 should handle sample input correctly") {
    assert(worker.part1(sample) === 136)
  }

  test("Part 2 should handle sample input correctly") {
    assert(worker.part2(sample) === 0)
  }
}
