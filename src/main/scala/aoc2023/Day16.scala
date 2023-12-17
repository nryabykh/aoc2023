package aoc2023

import scala.annotation.tailrec

object Day16 extends Day {
  override val dayNumber: String = "16"

  case class Point(x: Int, y: Int, direction: Char) {
    def getRight: Point = Point(x + 1, y, 'R')
    def getLeft: Point = Point(x - 1, y, 'L')
    def getUp: Point = Point(x, y - 1, 'U')
    def getDown: Point = Point(x, y + 1, 'D')
  }

  case class Game(lines: Seq[String]) {
    val width: Int = lines.head.length
    val height: Int = lines.length

    private def getNextPoints(point: Point): Seq[Point] = {
      val Point(x, y, direction) = point
      direction match {
        case 'R' => lines(y)(x) match {
          case '-' | '.' => Seq(point.getRight)
          case '|' => Seq(point.getUp, point.getDown)
          case '\\' => Seq(point.getDown)
          case '/' => Seq(point.getUp)
        }
        case 'L' => lines(y)(x) match {
          case '-' | '.' => Seq(point.getLeft)
          case '|' => Seq(point.getUp, point.getDown)
          case '/' => Seq(point.getDown)
          case '\\' => Seq(point.getUp)
        }
        case 'U' => lines(y)(x) match {
          case '|' | '.' => Seq(point.getUp)
          case '-' => Seq(point.getLeft, point.getRight)
          case '/' => Seq(point.getRight)
          case '\\' => Seq(point.getLeft)
        }
        case 'D' => lines(y)(x) match {
          case '|' | '.' => Seq(point.getDown)
          case '-' => Seq(point.getLeft, point.getRight)
          case '/' => Seq(point.getLeft)
          case '\\' => Seq(point.getRight)
        }
       }
    }

    @tailrec
    final def makeStep(queue: List[Point], visited: Set[Point]): Set[Point] = {
      if (queue.isEmpty) visited else {
        val currentPoint = queue.head
        val nextPoints: Seq[Point] = getNextPoints(currentPoint)
          .filter(p => (p.x > -1) && (p.x < width) && (p.y > -1) && (p.y < height))
          .filter(p => !visited.contains(p))

        val nextQueue = nextPoints.foldLeft(queue.tail) { case(acc, item) => item :: acc }
        val newVisited = visited + currentPoint

        makeStep(nextQueue, newVisited)
      }
    }

    def playRound(start: Point): Int = {
      makeStep(List(start), Set.empty[Point])
        .map(p => (p.x, p.y))
        .size
    }
  }

  override def part1(data: String): Int = {
    val lines = data.split("\n")
    val game = Game(lines)
    game.playRound(Point(0, 0, 'R'))
  }

  override def part2(data: String): Int = {
    val lines = data.split("\n")
    val game = Game(lines)

    val verticals = (0 until game.width).flatMap {x =>
      Seq(
        game.playRound(Point(x, 0, 'D')),
        game.playRound(Point(x, game.height - 1, 'U'))
      )
    }.max

    val horizontals = (0 until game.height).flatMap { y =>
      Seq(
        game.playRound(Point(0, y, 'R')),
        game.playRound(Point(game.width - 1, y, 'L'))
      )
    }.max

    Seq(verticals, horizontals).max
  }
}
