package com.github.nryabykh

import org.scalatest.funsuite.AnyFunSuite

class Day03Spec extends AnyFunSuite{
  private final val sample =
    """|467..114..
       |...*......
       |..35..633.
       |......#...
       |617*......
       |.....+.58.
       |..592.....
       |......755.
       |...$.*....
       |.664.598..
       |""".stripMargin

  private final val sampleReddit =
    """12.......*..
      |+.........34
      |.......-12..
      |..78........
      |..*....60...
      |78.........9
      |.5.....23..$
      |8...90*12...
      |............
      |2.2......12.
      |.*.........*
      |1.1..503+.56
      |""".stripMargin

  test("Part 1 should handle sample input correctly") {
    assert(Day03.part1(sample) === 4361)
    assert(Day03.part1(sampleReddit) === 925)
  }

  test("Part 2 should handle sample input correctly") {
    assert(Day03.part2(sample) === 467835)
    assert(Day03.part2(sampleReddit) === 6756)
  }

}
