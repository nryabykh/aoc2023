package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day12Spec extends AnyFunSuite {
  private val sample =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1
      |""".stripMargin

  private val worker = Day12

  test("Part 1 should handle sample input correctly") {
    assert(worker.part1(sample) === 21)
  }

  test("Part 2 should handle sample input correctly") {
    assert(worker.part2(sample) === 0)
  }
}
