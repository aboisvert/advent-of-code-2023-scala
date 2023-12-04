package day04

@main def part1 =
  val points = scala.io.Source
    .fromFile("day04/input.txt")
    .getLines
    .map(parseScratchcard)
    .map(_.points)
    .sum
  println(points)

case class Scratchcard(id: Int, winning: Set[Int], mine: Set[Int]):
  def points = math.pow(2, mine.count(winning) - 1).toInt
end Scratchcard

// Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
def parseScratchcard(line: String): Scratchcard =
  val Array(cardId, numbers) = line.split(""":\s+""")
  val Array(_, id) = cardId.split("""\s+""")
  val Array(winningNumbers, myNumbers) = numbers.split("""\s+\|\s+""")
  def parseNumbers(s: String) = s.split("""\s+""").map(_.toInt).toSet
  Scratchcard(
    id.toInt,
    winning = parseNumbers(winningNumbers),
    mine = parseNumbers(myNumbers)
  )
