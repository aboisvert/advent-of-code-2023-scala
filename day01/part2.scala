package day01

import scala.io.Source
import scala.util.boundary.*
import scala.util.boundary
import scala.util.ChainingOps

@main def part2 =
  val sum = Source
    .fromFile("day01/input.txt")
    .getLines
    .map(calibrationValuePart2)
    .sum
  println(sum)

def calibrationValuePart2(line: String): Int =
  val firstDigitAsNumber  = line.findFirstDigit
  val firstDigitAsLetters = line
    .findFirstWord(digitsAsLetters.keySet)
    .map { (index, word) => index -> digitsAsLetters(word) }
  val firstDigit = min(firstDigitAsNumber, firstDigitAsLetters)

  val lastDigitAsNumber  = line.reverse.findFirstDigit
  val lastDigitAsLetters = line.reverse
    .findFirstWord(reversedDigitsAsLetters.keySet)
    .map { (index, word) => index -> reversedDigitsAsLetters(word) }
  val lastDigit = min(lastDigitAsNumber, lastDigitAsLetters)

  (firstDigit.toString + lastDigit.toString).toInt
end calibrationValuePart2

val digitsAsLetters = Map(
  "one" -> 1,
  "two" -> 2,
  "three" -> 3,
  "four" -> 4,
  "five" -> 5,
  "six" -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine" -> 9
)

val reversedDigitsAsLetters = digitsAsLetters
  .map { (k, v) => k.reverse -> v }

extension (line: String)
  /** Returns (index, word) of the first matching word */
  def findFirstWord(words: Iterable[String]): Option[(Int, String)] =
    boundary:
      for
        fromIndex <- line.indices
        word <- words
      do
        if line.indexOf(word, fromIndex) == fromIndex then
          break(Some((fromIndex, word)))
      None
  end findFirstWord

  /** Returns (index, digit) of the first digit */
  def findFirstDigit: Option[(Int, Int)] =
    for
      digit <- line.find(Character.isDigit)
      index = line.indexOf(digit)
    yield (index, digit - '0')
  end findFirstDigit

/** Given a list of optional (Index, Value) tuples, return the value with minimal index */
def min(seq: Option[(Int, Int)]*): Int =
  seq.flatten.minBy(_._1)._2
end min
