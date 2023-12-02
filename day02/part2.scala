package day02

@main def part2 =
  val sum = scala.io.Source
    .fromFile("day02/input.txt")
    .getLines
    .map(parseGame)
    .map(_.power)
    .sum
  println(sum)

extension (g: Game)
  def power =
    g.picks.maxBy(_.red).red *
    g.picks.maxBy(_.green).green *
    g.picks.maxBy(_.blue).blue
  end power

