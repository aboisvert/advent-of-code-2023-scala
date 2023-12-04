package day04

@main def part1 =
  val points = scala.io.Source
    .fromFile("day04/input.txt")
    .getLines
    .map(parseScratchcard)
    .map(_.points)
    .sum
  println(points)

case class Scratchcard(id: Int, winningNumbers: Set[Int], myNumbers: Set[Int]):
  /** Returns points for this scratcard, which is 1 for 1 winning number, 2 for 2, 4 for 3, etc. */
  def points = math.pow(2, myNumbers.count(winningNumbers) - 1).toInt
end Scratchcard

def parseScratchcard(line: String): Scratchcard =
  // Format: "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
  val Array(cardId, numbers) = line.split(""":\s+""")
  val Array(_, id) = cardId.split("""\s+""")
  val Array(winningNumbers, myNumbers) = numbers.split("""\s+\|\s+""")
  def parseNumbers(s: String) = s.split("""\s+""").map(_.toInt).toSet
  Scratchcard(
    id.toInt,
    parseNumbers(winningNumbers),
    parseNumbers(myNumbers)
  )