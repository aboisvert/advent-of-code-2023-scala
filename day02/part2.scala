package day02

@main def part2 =
  val sum = scala.io.Source
    .fromFile("day02/input.txt")
    .getLines
    .map(parseGame)
    .map { _.power }
    .sum
  println(sum)

extension (g: Game)
  def power =
    val maxPick = g.picks.reduce: (p1, p2) =>
      Pick(
        red = math.max(p1.red, p2.red),
        green = math.max(p1.green, p2.green),
        blue = math.max(p1.blue, p2.blue),
      )
    maxPick.red * maxPick.green * maxPick.blue

