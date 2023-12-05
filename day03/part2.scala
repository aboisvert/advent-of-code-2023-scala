package day03

@main def part2 =
  val grid: Array[Array[Char]] = scala.io.Source
    .fromFile("day03/input.txt")
    .getLines
    .toArray
    .map(_.toArray)
  val sum = Schematic(grid).validGears
    .map(_.ratio)
    .sum
  println(sum)

case class Gear(x: Int, y: Int)

/** A gear with exactly two adjacent parts */
case class ValidGear(gear: Gear, adjacentParts: Seq[PartNumber]):
  assert(adjacentParts.size == 2)
  export gear.x, gear.y
  def ratio = adjacentParts(0).digits.toInt * adjacentParts(1).digits.toInt
end ValidGear

extension (part: PartNumber)
  /** Returns true if `gear` is adjacent to this part number, false otherwise */
  def isAdjacentTo(gear: Gear) =
    ((part.y >= gear.y - 1) && (part.y <= gear.y + 1)) &&
      ((part.x + part.digits.length >= gear.x) && (part.x <= gear.x + 1))

extension (gear: Gear)
  /** Returns all the adjacent parts to this gear */
  def adjacentParts(validParts: Iterable[PartNumber]): Iterable[PartNumber] =
    for part <- validParts if part.isAdjacentTo(gear)
    yield part

extension (schematic: Schematic)
  def validGears: Iterator[ValidGear] =
    val parts = schematic.validPartNumbers.toSeq
    for
      (line, y) <- schematic.grid.iterator.zipWithIndex
      x <- line.indices
      gear = Gear(x, y) if schematic.charAt(x, y) == '*'
      adjacent = gear.adjacentParts(parts) if adjacent.size == 2
    yield ValidGear(gear, adjacent.toSeq)
