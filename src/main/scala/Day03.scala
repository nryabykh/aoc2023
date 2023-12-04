package com.github.nryabykh.aoc2023

object Day03 {

  case class Input(data: String) {
    val lines: Array[String] = data.split("\n")
    val linesWithIndex: Array[(String, Int)] = lines.zipWithIndex
    val inputChars: Array[Array[Char]] = lines.map(line => line.toCharArray)
    val width: Int = lines.head.length
    val height: Int = lines.length
  }

  case class Coord(x: Int, y: Int) {
    def allAdjacent: Seq[Coord] = {
      (x - 1 to x + 1).flatMap { i =>
        (y - 1 to y + 1).map(Coord(i, _))
      }.filterNot(c => (c.x == x) && (c.y == y))
    }

    def getCorrectAdjacent(width: Int, height: Int): Seq[Coord] = {
      allAdjacent.filter { case Coord(i, j) => (i > -1) && (i < width) && (j > -1) && (j < height) }
    }
  }

  case class CoordNumber(coord: Coord, value: Int) {
    def getAdjacent(width: Int, height: Int): Seq[Coord] = {
      (coord.x until coord.x + value.toString.length)
        .flatMap(i => Coord(i, coord.y).getCorrectAdjacent(width, height))
        .toSet.toSeq
    }
  }

  def getNumberCoords(line: String, y: Int): Seq[(Coord, CoordNumber)] = {
    val numberRegex = """\d+""".r
    numberRegex.findAllMatchIn(line)
      .map(m => (m.start, m.group(0)))
      .flatMap { case (start, number) =>
        (start until start + number.length)
          .map(x => Coord(x, y) -> CoordNumber(Coord(start, y), number.toInt))
      }
      .toSeq
  }

  def part1(data: String): Int = {
    val input = Input(data)

    def isSymbol(c: Coord): Boolean = {
      val char = input.inputChars(c.y)(c.x)
      !(char.isDigit || (char == '.'))
    }

    input.linesWithIndex.flatMap { case(line, y) =>
      getNumberCoords(line, y)
        .map { case (_, posNumber) => posNumber }
        .distinct
        .filter(_.getAdjacent(input.width, input.height).exists(isSymbol))
    }.map(_.value).sum
  }

  def part2(data: String): Int = {
    val input = Input(data)

    val numbersMap = input.linesWithIndex
      .flatMap { case (line, ix) => getNumberCoords(line, ix) }
      .toMap

    input.linesWithIndex
      .flatMap { case (line, ix) => """\*""".r.findAllMatchIn(line).map(x => Coord(x.start, ix)).toList }
      .map { coord =>
        coord.getCorrectAdjacent(input.width, input.height)
          .flatMap(numbersMap.get)
          .toSet
      }
      .collect { case s if s.size == 2 => s.toList.map(_.value).product }
      .sum
  }


  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("day03.txt").mkString
    println(part1(data))
    println(part2(data))
  }

}
