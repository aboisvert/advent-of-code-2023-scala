package day09

@main def part2 =
  val input = scala.io.Source.fromFile("day09/input.txt").getLines()
  // just reverse() the sequence to extrapolate *before* the sequence
  val sequences = input.map(parseSequence).map(_.reverse)
  val extrapolations = sequences.map(extrapolate)
  val sum = extrapolations.map(_.last).sum
  println(sum)
