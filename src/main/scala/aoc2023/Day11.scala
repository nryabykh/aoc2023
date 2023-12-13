package aoc2023

import scala.annotation.tailrec

object Day11 extends Day {
  override val dayNumber: String = "11"
  override val title: String = "Cosmic Expansion"
  override val link: String = "https://adventofcode.com/2023/day/11"


  def getAdj(x: Int, y: Int, width: Int, height: Int): Seq[(Int, Int)] = {
    Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
      .filter { case(i, j) => (i > -1) && (i < width) && (j > -1) && (j < height)}
  }

  @tailrec
  def makeStep(queue: List[(Int, Int, Long, Set[(Int, Int)])], target: (Int, Int), width: Int, height: Int): (Long, Set[(Int, Int)]) = {
    val (x, y, steps, visited) = queue.head
    val adj = getAdj(x, y, width, height).filter { case(i, j) => !visited.contains((i, j))}
    if (adj.contains(target)) (steps + 1, visited + ((x, y))) else {
      val (_, nextX, nextY) = adj.map { case (i, j) => (Math.abs(target._1 - i) + Math.abs(target._2 - j), i, j) }.min
      val newVisited: Set[(Int, Int)] = visited + ((x, y))
      makeStep((nextX, nextY, steps + 1, newVisited) :: queue.tail, target, width, height)
    }
  }

  def checkRow(line: String): Boolean = line.toCharArray.distinct sameElements ".".toCharArray
  def checkColumn(i: Int, lines: Seq[String]): Boolean = lines.map(x => x(i)).distinct sameElements ".".toCharArray

  def findDistances(lines: Array[String], multiplier: Int): Seq[Long] = {
    val rowIndices = lines.zipWithIndex.collect { case (line, j) if (checkRow(line)) => j }.toSet
    val colIndices = (0 until lines.head.length).filter(checkColumn(_, lines)).toSet

    val (width, height) = (lines.head.length, lines.length)

    val starts: Seq[(Int, Int)] = lines.zipWithIndex
      .flatMap { case (line, ix) => """#""".r.findAllMatchIn(line).map(m => (m.start, ix)) }
      .sorted

    val combinations = starts.flatMap { case (x, y) => starts
      .filter { case (i, j) => (i, j) != (x, y) }
      .map { case (i, j) => List((x, y), (i, j)).sorted }
    }.distinct

    combinations.map { item =>
      val (x1, y1) :: (x2, y2) :: _ = item
      val (distance, visited) = makeStep(List((x1, y1, 0, Set.empty[(Int, Int)])), (x2, y2), width, height)
      visited.foldLeft(distance) { case (acc, (x, y)) =>
        acc + Seq(rowIndices.contains(y), colIndices.contains(x)).count(_ == true) * multiplier
      }
    }
  }

  override def part1(data: String): AnyVal = {
    val lines = data.split("\n")
    findDistances(lines, 1).sum
  }

  override def part2(data: String): AnyVal = {
    val lines = data.split("\n")
    findDistances(lines, 1000000 - 1).sum
  }
}
