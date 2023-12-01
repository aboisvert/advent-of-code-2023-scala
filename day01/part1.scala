package day01

import scala.io.Source

@main def part1 =
  val sum = Source
    .fromFile("day01/input.txt")
    .getLines
    .map(calibrationValue)
    .sum
  println(sum)

def calibrationValue(line: String): Int =
  val firstDigit = line.find(Character.isDigit).get
  val lastDigit  = line.findLast(Character.isDigit).get
  (firstDigit.toString + lastDigit).toInt
