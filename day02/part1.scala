package day02

@main def part1 =
  val sum = scala.io.Source
    .fromFile("day02/input.txt")
    .getLines
    .map(parseGame)
    .filter: game =>
      game.picks.forall: pick =>
        pick.red <= 12 &&
        pick.green <= 13 &&
        pick.blue <= 14
    .map { _.id }
    .sum
  println(sum)

case class Game(id: Int, picks: Seq[Pick])

case class Pick(red: Int, green: Int, blue: Int)

// Game 92: 1 green, 3 red, 1 blue; 2 blue, 2 green, 5 red; 2 blue, 8 red; 1 blue, 2 green, 14 red; 3 red; 1 blue, 9 red
def parseGame(line: String): Game =
  // println(line)
  val Array(gameStr, picksStr) = line.split(":")

  val picks =
    for
      pickSet <- picksStr.split(";")
    yield
      // println(pickSet.toSeq)
      val picks = for
        randomCubes <- pickSet.split(",")
        Array(n, color) = randomCubes.trim.split(" ")
      yield (color, n.toInt)
      val picksMap = picks.toMap
      // println(picksMap)
      Pick(
        red   = picksMap.getOrElse("red", 0),
        green = picksMap.getOrElse("green", 0),
        blue  = picksMap.getOrElse("blue", 0),
      )

  // println(picks.toSeq)
  val Array(_, id) = gameStr.split(" ")

  Game(id.toInt, picks)