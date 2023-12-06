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

def distanceTravelledAt(time: Int, pushTime: Int) =
  if time <= pushTime then 0
  else (time - pushTime) * (pushTime)

case class Race(raceTime: Int, recordDistance: Int):
  def winningSolutions: Int =
    (0 to raceTime).count: pushTime =>
      distanceTravelledAt(time = raceTime, pushTime) > recordDistance

def parseRaces(lines: Iterator[String]): Iterable[Race] =
  val times = lines.next().split("""\s+""").drop(1).map(_.toInt)
  val distances = lines.next().split("""\s+""").drop(1).map(_.toInt)
  times
    .zip(distances)
    .map { (time, distance) => Race(time, distance) }
