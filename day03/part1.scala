package day03

@main def part1 =
  val grid: Array[Array[Char]] = scala.io.Source
    .fromFile("day03/input.txt")
    .getLines
    .toArray
    .map(_.toArray)
  val sum = Schematic(grid).validPartNumbers
    .map(_.digits.toInt)
    .sum
  println(sum)

case class Schematic(grid: Array[Array[Char]]):
  /** Returns the character at position (x, y), or '.' as default */
  def charAt(x: Int, y: Int): Char =
    if x < 0 || y < 0 || y >= grid.length || x >= grid(y).length then '.'
    else grid(y)(x)

  /** Returns the PartNumber at (x, y), or None */
  def possiblePartNumberAt(x: Int, y: Int): Option[PartNumber] =
    if charAt(x, y).isDigit && !charAt(x - 1, y).isDigit then
      val digits = grid(y).iterator.drop(x).takeWhile(_.isDigit).toArray
      Some(PartNumber(String.valueOf(digits), x, y))
    else None

  /** Returns an iterator of all PartNumbers in the schematic */
  def validPartNumbers: Iterator[PartNumber] =
    for
      (line, y) <- grid.iterator.zipWithIndex
      x <- line.indices
      part <- possiblePartNumberAt(x, y) if part.hasAdjacentSymbol(using this)
    yield part

end Schematic

extension (c: Char)
  /** A char is a symbol if it's not a digit and not a dot ('.') */
  def isSymbol = !c.isDigit && !(c == '.')

case class PartNumber(digits: String, x: Int, y: Int):
  /** Returns true if this part number has an adjacent symbol, false otherwise
    */
  def hasAdjacentSymbol(using schematic: Schematic): Boolean =
    val adjacentChars = for
      x1 <- ((x - 1) to (x + digits.size)).iterator
      y1 <- (y - 1) to (y + 1)
    yield schematic.charAt(x1, y1)
    adjacentChars.exists(_.isSymbol)
end PartNumber
