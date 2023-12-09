package aoc2023

object Day09 extends Day {
  override val dayNumber: String = "09"
  override val title: String = "Mirage Maintenance"
  override val link: String = "https://adventofcode.com/2023/day/9"

  
  private def parseLine: String => Seq[Long] = line => line.split(" ").map(_.toLong)

  private def getDiffs(nums: Seq[Long]): Seq[Long] = {
    nums.zip(nums.drop(1)).map { case (first, second) => second - first }
  }

  private def extendLast: Seq[Long] => Seq[Long] = nums => {
    if (nums.min == nums.max) Seq(nums.min) else {
      nums.appended(nums.last + extendLast(getDiffs(nums)).last)
    }
  }

  private def extendFirst: Seq[Long] => Seq[Long] = nums => {
    if (nums.min == nums.max) Seq(nums.min) else {
      nums.prepended(nums.head - extendFirst(getDiffs(nums)).head)
    }
  }

  override def part1(data: String): AnyVal = {
    data.split("\n")
      .map(x => (parseLine andThen extendLast)(x))
      .map(_.last)
      .sum
  }

  override def part2(data: String): AnyVal = {
    data.split("\n")
      .map(x => (parseLine andThen extendFirst) (x))
      .map(_.head)
      .sum
  }
}
