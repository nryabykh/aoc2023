package aoc2023

import org.scalatest.funsuite.AnyFunSuite

class Day10Spec extends AnyFunSuite {
  private val sample =
    """.....
      |.S-7.
      |.|.|.
      |.L-J.
      |.....
      |""".stripMargin

  private val worker = Day10

  test("Part 1 should handle sample input correctly") {

    val sampleTwo =
      """..F7.
        |.FJ|.
        |SJ.L7
        ||F--J
        |LJ...
        |""".stripMargin

    val sampleThree =
      """-L|F7
        |7S-7|
        |L|7||
        |-L-J|
        |L|-JF
        |""".stripMargin

    assert(worker.part1(sample) === 4)
    assert(worker.part1(sampleTwo) === 8)
    assert(worker.part1(sampleThree) === 4)
  }

  test("Part 2 should handle sample input correctly") {
    val sampleTwo =
      """...........
        |.S-------7.
        |.|F-----7|.
        |.||.....||.
        |.||.....||.
        |.|L-7.F-J|.
        |.|..|.|..|.
        |.L--J.L--J.
        |...........
        |""".stripMargin

    val sampleThree =
      """.F----7F7F7F7F-7....
        |.|F--7||||||||FJ....
        |.||.FJ||||||||L7....
        |FJL7L7LJLJ||LJ.L-7..
        |L--J.L7...LJS7F-7L7.
        |....F-J..F7FJ|L7L7L7
        |....L7.F7||L7|.L7L7|
        |.....|FJLJ|FJ|F7|.LJ
        |....FJL-7.||.||||...
        |....L---J.LJ.LJLJ...
        |""".stripMargin

    val sampleFour =
      """FF7FSF7F7F7F7F7F---7
        |L|LJ||||||||||||F--J
        |FL-7LJLJ||||||LJL-77
        |F--JF--7||LJLJ7F7FJ-
        |L---JF-JLJ.||-FJLJJ7
        ||F|F-JF---7F7-L7L|7|
        ||FFJF7L7F-JF7|JL---7
        |7-L-JL7||F7|L7F-7F7|
        |L.L7LFJ|||||FJL7||LJ
        |L7JLJL-JLJLJL--JLJ.L
        |""".stripMargin

    assert(worker.part2(sampleTwo) === 4)
    assert(worker.part2(sampleThree) === 8)
    assert(worker.part2(sampleFour) === 10)
  }
}
