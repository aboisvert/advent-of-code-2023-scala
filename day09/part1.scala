package day09

@main def part1 =
  val input = scala.io.Source.fromFile("day09/input.txt").getLines()
  val sequences = input.map(parseSequence)
  val extrapolations = sequences.map(extrapolate)
  val sum = extrapolations.map(_.last).sum
  println(sum)

def parseSequence(line: String): Seq[Long] =
  line.split("""\s+""").map(_.toLong).toSeq

def extrapolate(seq: Seq[Long]): Seq[Long] =
  if seq.forall(_ == 0) then return (seq :+ 0L)
  val nextLevel = seq.sliding(2).fold(Seq.empty[Long]): (seq, tuple) =>
    (seq :+ (tuple(1) - tuple(0)))
  val extrapolated = extrapolate(nextLevel)
  seq :+ (extrapolated.last + seq.last)
