package day06

import scala.io.Source

@main def part2 =
  val lines = scala.io.Source
    .fromFile("day06/input.txt")
    .getLines
  val race = parseRaceWithBadKerning(lines)
  println(race.winningSolutions)
  println(race.optimizedWinningSolutions)

def parseRaceWithBadKerning(lines: Iterator[String]): Race =
  Race(
    raceTime = lines.next().split("""\s+""").drop(1).mkString.toLong,
    recordDistance = lines.next().split("""\s+""").drop(1).mkString.toLong
  )

extension (r: Race)
  /** Returns the count of winning solutions using algebraic optimization. */
  def optimizedWinningSolutions =
    // system of equations is:
    //
    //  #1  0 >= pushTime >= raceTime
    //  #2  (raceTime - pushTime) * pushTime > recordDistance
    //
    //  solve for `pushtime`:
    //
    //  1. Distribute `p` on (raceTime - pushTime):
    //
    //    (raceTime * pushTime) - pushTime^2 > recordDistance
    //
    //  2. Substract recordDistance on both sides
    //
    //    (raceTime * pushTime) - pushTime^2 - recordDistance = 0
    //
    //  3. This is now a quadratic equation of the form (ax^2 + bx + c = 0)
    //
    //       a = -1
    //       b = raceTime
    //       c = -recordDistance
    //
    //     pushTime = -b +/- sqrt(b^2 - 4ac) / 2a
    //
    //     pushTime =  -raceTime +/- sqrt(raceTime^2 - (4 * -1 * -recordDistance)) / ( 2 * -1)
    //              =  -racetime +/- sqrt(raceTime^2 - (4 * recordDistance)) / -2

    import r.{raceTime, recordDistance}
    import math.{abs, sqrt, pow}

    val solution1 =
      -raceTime + sqrt(pow(raceTime, 2.0d) - (4.0d * recordDistance)) / -2.0d
    val solution2 =
      -raceTime - sqrt(pow(raceTime, 2.0d) - (4.0d * recordDistance)) / -2.0d

    solution2.floor.toLong - solution1.ceil.toLong
