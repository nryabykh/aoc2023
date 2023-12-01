package com.github.nryabykh

import org.scalatest.funsuite.AnyFunSuite

class Day01Spec extends AnyFunSuite {

  test("Part 1 should handle sample input correctly") {
    val sample =
      """|1abc2
         |pqr3stu8vwx
         |a1b2c3d4e5f
         |treb7uchet
         |""".stripMargin

    assert(Day01.part1(sample) === 142)
  }

  test("Part 2 should handle sample input correctly") {
    val sample2 =
      """|two1nine
        |eightwothree
        |abcone2threexyz
        |xtwone3four
        |4nineeightseven2
        |zoneight234
        |7pqrstsixteen
        |""".stripMargin

    assert(Day01.part2(sample2) === 281)
  }
}
