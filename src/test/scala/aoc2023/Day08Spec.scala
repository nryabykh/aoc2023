package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day08Spec extends AnyFunSuite{

  test("Part 1 should handle sample input correctly") {
    val sample =
      """RL
        |
        |AAA = (BBB, CCC)
        |BBB = (DDD, EEE)
        |CCC = (ZZZ, GGG)
        |DDD = (DDD, DDD)
        |EEE = (EEE, EEE)
        |GGG = (GGG, GGG)
        |ZZZ = (ZZZ, ZZZ)
        |""".stripMargin

    val sampleTwo =
      """LLR
        |
        |AAA = (BBB, BBB)
        |BBB = (AAA, ZZZ)
        |ZZZ = (ZZZ, ZZZ)
        |""".stripMargin

    assert(Day08.part1(sample) === 2)
    assert(Day08.part1(sampleTwo) === 6)
  }

  test("Part 2 should handle sample input correctly") {
    val samplePart2 =
      """LR
        |
        |11A = (11B, XXX)
        |11B = (XXX, 11Z)
        |11Z = (11B, XXX)
        |22A = (22B, XXX)
        |22B = (22C, 22C)
        |22C = (22Z, 22Z)
        |22Z = (22B, 22B)
        |XXX = (XXX, XXX)
        |""".stripMargin

    assert(Day08.part2(samplePart2) === 6)
  }

}
