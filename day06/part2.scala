package day06

import scala.io.Source

@main def part2 =
  val lines = scala.io.Source
    .fromFile("day06/input.txt")
    .getLines
  val race = parseRaceWithBadKerning(lines)
  println(race.winningSolutions)

def parseRaceWithBadKerning(lines: Iterator[String]): Race =
  Race(
    raceTime = lines.next().split("""\s+""").drop(1).mkString.toLong,
    recordDistance = lines.next().split("""\s+""").drop(1).mkString.toLong
  )

