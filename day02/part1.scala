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
end part1

case class Game(id: Int, picks: Seq[Pick])

case class Pick(red: Int, green: Int, blue: Int)

/** Parse game line
  *
  * Sample: "Game 92: 1 green, 3 red, 1 blue; 2 blue, 2 green, 5 red; 2 blue, 8
  * red; 1 blue, 2 green, 14 red; 3 red; 1 blue, 9 red"
  */
def parseGame(line: String): Game =
  val Array(gameStr, picksStr) = line.split(":")
  val Array(_, id) = gameStr.split(" ")
  val picks = for pickSet <- picksStr.split(";") yield
    val colorMap = (for
      randomCubes <- pickSet.split(",")
      Array(n, color) = randomCubes.trim.split(" ")
    yield (color, n.toInt)).toMap
    Pick(
      red = colorMap.getOrElse("red", 0),
      green = colorMap.getOrElse("green", 0),
      blue = colorMap.getOrElse("blue", 0)
    )
  Game(id.toInt, picks)
end parseGame