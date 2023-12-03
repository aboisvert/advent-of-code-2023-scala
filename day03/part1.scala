package day03

@main def part1 =
  val grid: Array[Array[Char]] = scala.io.Source
    .fromFile("day03/input.txt")
    .getLines
    .toArray
    .map(_.toArray)
  val parts = validPartNumbers(using Schematic(grid))
  println(parts.map(_.digits.toInt).sum)

case class Schematic(grid: Array[Array[Char]]):
  def charAt(x: Int, y: Int): Char =
    if x < 0 || y < 0 || y >= grid.length || x >= grid(y).length then '.'
    else grid(y)(x)
end Schematic

extension (c: Char) def isSymbol = !c.isDigit && !(c == '.')

case class PartNumber(digits: String, x: Int, y: Int):
  def hasAdjacentSymbol(using schematic: Schematic): Boolean =
    (for
      x1 <- (x - 1) to (x + digits.size)
      y1 <- (y - 1) to (y + 1)
    yield schematic.charAt(x1, y1))
      .exists(_.isSymbol)
end PartNumber

def possiblePartNumberAt(x: Int, y: Int)(using
    schematic: Schematic
): Option[PartNumber] =
  if schematic.charAt(x, y).isDigit && !schematic.charAt(x - 1, y).isDigit then
    val digits = schematic.grid(y).iterator.drop(x).takeWhile(_.isDigit).toArray
    Some(PartNumber(String.valueOf(digits), x, y))
  else None

def validPartNumbers(using schematic: Schematic): Iterator[PartNumber] =
  for
    (line, y) <- schematic.grid.iterator.zipWithIndex
    x <- line.indices
    part <- possiblePartNumberAt(x, y) if part.hasAdjacentSymbol
  yield part
