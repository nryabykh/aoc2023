package aoc2023

import scala.annotation.tailrec

object Day12 extends Day {
  override val dayNumber: String = "12"

  def parse(line: String): (String, Seq[Int]) = {
    val groups = line.split(" ").last.split(",").map(_.toInt).toSeq
    val row = line.split(" ").head
    (row, groups)
  }

  def check(row: String, groups: Seq[Int]): Boolean = {
    row.split("\\.").filter(_.nonEmpty).map(_.length).toSeq == groups
  }

  def calcQm(line: String): Int = {
    line.toCharArray.count(_ == '?')
  }

  @tailrec
  def expand(lines: Seq[String], ix: Int, indices: Seq[Int], initLine: String): Seq[String] = {
    if (ix == initLine.length) lines else {
      val newLines = if (indices.contains(ix)) {
        lines.flatMap(l => Seq(s"$l.", s"$l#"))
      } else {
        lines.map(l => s"$l${initLine(ix)}")
      }
      expand(newLines, ix + 1, indices, initLine)
    }
  }

  def getArrangements(line: String): Seq[String] = {
    val (row, groups) = parse(line)
    val qmIndices = row.toCharArray.zipWithIndex.collect { case(ch, ix) if ch == '?' => ix }

    val res = row.indices.foldLeft(Seq.empty[String]) { case(acc, ix) =>
      val newAcc = if (qmIndices.contains(ix)) {
        if (acc.isEmpty) Seq(".", "#") else acc.flatMap(l => Seq(s"$l.", s"$l#"))
      } else {
        if (acc.isEmpty) Seq(row(ix).toString) else acc.map(l => s"$l${row(ix)}")
      }
      newAcc
    }.filter(check(_, groups))

    res
  }

  override def part1(data: String): AnyVal = {
    data.split("\n")
      .map(line => getArrangements(line).size)
      .sum
  }

  override def part2(data: String): AnyVal = {
    0
  }
}
