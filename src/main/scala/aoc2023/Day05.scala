package aoc2023

object Day05 extends Day {
  override val dayNumber: String = "05"
  override val title: String = "If You Give A Seed A Fertilizer"

  private def toRangeMap: String => RangeMap = s => {
    val Array(dest, source, length) = s.split(" ").map(_.toLong)
    RangeMap(Range(source, length), dest)
  }

  override def part1(data: String): Long = {
    val input = data.split("\n\n")
    val seeds = input.head
      .split(": ").last
      .split(" ").map(_.toLong).toSeq

    val maps = input.drop(1)

    maps.foldLeft(seeds) {
      case (acc, currentMap) =>
        val mapLines = currentMap.split("\n").drop(1).map(toRangeMap)
        acc.map(item => mapLines
          .find(t => (t.startSource <= item) && (item < t.startSource + t.length))
          .map(t => t.startDest + (item - t.startSource))
          .getOrElse(item))
    }.min
  }

  override def part2(data: String): Long = {
    val input = data.split("\n\n")
    val seedsRange = input.head.split(": ").last

    val seeds = """(\d+)\s(\d+)""".r.findAllMatchIn(seedsRange)
      .map(m => Range(m.group(1).toLong, m.group(2).toLong))
      .toSeq

    val maps = input.drop(1)

    maps.foldLeft(seeds) {
      case (acc, currentMap) =>
        val mapLines: Seq[RangeMap] = currentMap.split("\n").drop(1).map(toRangeMap)
        acc.flatMap { seed =>
          val intersections: Seq[(Range, Long)] = mapLines
            .map(map => (seed.intersect(map.range), map.startDest - map.startSource))
            .collect { case (x, delta) if x.nonEmpty => (x.move(delta), delta) }

          val result: Seq[(Range, Long)] = if (intersections.nonEmpty) {
            val intersectedMin = intersections.map { case(r, delta) => r.start - delta }.min
            val intersectedMax = intersections.map { case(r, delta) => r.start + r.length - delta }.max
            (intersections
              ++ Seq((Range(seed.start, intersectedMin - seed.start), 0L))
              ++ Seq((Range(seed.start + seed.length, intersectedMax - (seed.start + seed.length)), 0L)))
              .filter { case(r, _) => r.length > 0 }
          } else Seq((seed, 0L))
          result.map { case(r, _) => r }
        }
    }
      .map(_.start).min
  }
}

case class Range(start: Long, length: Long) {
  def intersect(other: Range): Range = {
    val startValue = Math.max(start, other.start)
    val endValue = Math.min(start + length, other.start + other.length)
    Range(startValue, Math.max(endValue - startValue, 0))
  }

  def move(delta: Long): Range = {
    Range(start + delta, length)
  }

  def isEmpty: Boolean = length == 0
  def nonEmpty: Boolean = !isEmpty
}

case class RangeMap(range: Range, startDest: Long) {
  val startSource: Long = range.start
  val length: Long = range.length
}
