package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day13Spec extends AnyFunSuite {
  private val sample =
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#
      |""".stripMargin

  private val worker = Day13

  test("Part 1 should handle sample input correctly") {
    assert(worker.part1(sample) === 405)
  }

  test("Part 2 should handle sample input correctly") {
    assert(worker.part2(sample) === 400)
  }
}
