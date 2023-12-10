package aoc2023

import scala.annotation.tailrec

object Day10 extends Day {
  override val dayNumber: String = "10"

  case class Point(x: Int, y: Int)

  case class Game(data: String) {
    val lines: Array[String] = data.split("\n")
    val linesWithIndex: Array[(String, Int)] = lines.zipWithIndex
    val inputChars: Array[Array[Char]] = lines.map(line => line.toCharArray)
    val width: Int = lines.head.length
    val height: Int = lines.length
    val points: Map[Point, Char] = {
      linesWithIndex.flatMap { case(line, y) =>
        line.zipWithIndex.map { case(ch, x) =>
          (Point(x, y), ch)
        }
      }.toMap
    }
    val start: Point = points.collect { case(k, v) if v == 'S' => k }.head

    val possible: Map[Char, String] = Map('U' -> "7|F", 'D' -> "J|L", 'L' -> "L-F", 'R' -> "J-7")
  }

  def getNextPoint(p: Point, char: Char): Seq[(Point, Char)] = {
    char match {
      case '-' => Seq((Point(p.x - 1, p.y), 'L'), (Point(p.x + 1, p.y), 'R'))
      case '|' => Seq((Point(p.x, p.y - 1), 'U'), (Point(p.x, p.y + 1), 'D'))
      case 'J' => Seq((Point(p.x, p.y - 1), 'U'), (Point(p.x - 1, p.y), 'L'))
      case '7' => Seq((Point(p.x, p.y + 1), 'D'), (Point(p.x - 1, p.y), 'L'))
      case 'L' => Seq((Point(p.x + 1, p.y), 'R'), (Point(p.x, p.y - 1), 'U'))
      case 'F' => Seq((Point(p.x + 1, p.y), 'R'), (Point(p.x, p.y + 1), 'D'))
      case 'S' => Seq((Point(p.x - 1, p.y), 'L'), (Point(p.x + 1, p.y), 'R'), (Point(p.x, p.y - 1), 'U'), (Point(p.x, p.y + 1), 'D'))
      case _ => Seq.empty[(Point, Char)]
    }
  }

  def isInBounds(point: Point, width: Int, height: Int): Boolean = {
    (point.x > -1) && (point.x < width) && (point.y > -1) && (point.y < height)
  }

  def isPossible(point: Point, direction: Char, game: Game): Boolean = {
    val char = game.inputChars(point.y)(point.x)
    game.possible(direction).contains(char) || (char == 'S')
  }

  def isNotVisited(p: Point, visited: Set[Point], game: Game): Boolean = {
    (game.inputChars(p.y)(p.x) == 'S') || (!visited.contains(p))
  }

  @tailrec
  def makeStep(queue: List[(Point, Point, Int, Set[Point])], game: Game): Set[Point] = {
    val (currentPoint, prevPoint, stepCnt, visited) :: tail = queue
    val char = game.inputChars(currentPoint.y)(currentPoint.x)

    if ((char == 'S') && visited.nonEmpty) visited else {
      val next = getNextPoint(currentPoint, char)
        .filter { case(p, _) => isInBounds(p, game.width, game.height) && (p != prevPoint) }
        .filter { case(p, d) => isPossible(p, d, game) }
        .filter { case(p, _) => isNotVisited(p, visited, game) }
        .map { case(p, _) => p }

      val newQueue = next.foldLeft(tail) { case(acc, nextItem) =>
        (nextItem, currentPoint, stepCnt + 1, visited + currentPoint) :: acc }
      makeStep(newQueue, game)
    }

  }

  override def part1(data: String): AnyVal = {
    val game = Game(data)
    makeStep(List((game.start, Point(-1, -1), 0, Set.empty[Point])), game).size / 2
  }

  override def part2(data: String): AnyVal = {
    val game = Game(data)
    val path = makeStep(List((game.start, Point(-1, -1), 0, Set.empty[Point])), game)

    game.inputChars.zipWithIndex.flatMap { case(line, j) =>
      line.indices
        .filter(i =>  !path.contains(Point(i, j)))
        .filter { i =>
          val crosses = if (!line.slice(i + 1, game.width).contains('S')) {
            ((i + 1) until game.width).count(x => path.contains(Point(x, j)) && "7|F".contains(line(x)))
          } else {
            (0 until i).count(x => path.contains(Point(x, j)) && "7|F".contains(line(x)))
          }
          crosses % 2 == 1
        }
    }.length
  }
}
