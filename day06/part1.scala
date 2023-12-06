package day06

import scala.io.Source

@main def part1 =
  val lines = scala.io.Source
    .fromFile("day06/input.txt")
    .getLines
  val races = parseRaces(lines)
  val total = races
    .map(_.winningSolutions)
    .reduce(_ * _)
  println(total)

def distanceTravelledAt(time: Long, pushTime: Long) =
  if time <= pushTime then 0
  else (time - pushTime) * (pushTime)

case class Race(raceTime: Long, recordDistance: Long):
  def winningSolutions: Long =
    (0L to raceTime).count: pushTime =>
      distanceTravelledAt(time = raceTime, pushTime) > recordDistance

def parseRaces(lines: Iterator[String]): Iterable[Race] =
  val times = lines.next().split("""\s+""").drop(1).map(_.toLong)
  val distances = lines.next().split("""\s+""").drop(1).map(_.toLong)
  times
    .zip(distances)
    .map { (time, distance) => Race(time, distance) }
