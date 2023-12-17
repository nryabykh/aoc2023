package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day15Spec extends AnyFunSuite {
  private val sample =
    """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
      |""".stripMargin

  private val worker = Day15

  test("Part 1 should handle sample input correctly") {
    assert(worker.part1(sample) === 1320)
  }

  test("Part 2 should handle sample input correctly") {
    assert(worker.part2(sample) === 145)
  }
}
