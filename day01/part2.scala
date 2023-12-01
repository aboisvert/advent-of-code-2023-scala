package day01

@main def part2 =
  val sum = scala.io.Source
    .fromFile("day01/input.txt")
    .getLines
    .map(calibrationValuePart2)
    .sum
  println(sum)

case class Numeral(chars: String, value: Int)

val digitsAsLetters = Set(
  Numeral("one", 1),
  Numeral("two", 2),
  Numeral("three", 3),
  Numeral("four", 4),
  Numeral("five", 5),
  Numeral("six", 6),
  Numeral("seven", 7),
  Numeral("eight", 8),
  Numeral("nine", 9)
)

// Add literal digits: ("1" -> 1), ("2" -> 2), etc.
val numerals = digitsAsLetters
  ++ digitsAsLetters.map { n => Numeral(n.chars, n.value) }

def calibrationValuePart2(line: String): Int =
  val left = numerals.minBy: n =>
    line.indexOf(n.chars) match
      case -1 => Int.MaxValue
      case i  => i
  val right = numerals.maxBy { n => line.lastIndexOf(n.chars) }
  (left.value * 10) + right.value
