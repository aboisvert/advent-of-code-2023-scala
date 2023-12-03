package day03

@main def part2 =
  val grid: Array[Array[Char]] = scala.io.Source
    .fromFile("day03/input.txt")
    .getLines
    .toArray
    .map(_.toArray)
  given Schematic = Schematic(grid)
  val gears = validGears.toSeq
  println(gears.map(_.ratio).sum)

case class ValidParts(parts: Seq[PartNumber])

case class Gear(x: Int, y: Int)

case class ValidGear(gear: Gear, adjacentParts: Seq[PartNumber]):
  export gear.x, gear.y
  def ratio = adjacentParts(0).digits.toInt * adjacentParts(1).digits.toInt
end ValidGear

extension (part: PartNumber)
  def isAdjacentTo(gear: Gear) =
    val result = ((part.y >= gear.y - 1) && (part.y <= gear.y + 1)) &&
      ((part.x + part.digits.length >= gear.x) && (part.x <= gear.x + 1))
    result

extension (gear: Gear)
  def adjacentParts(using
      schematic: Schematic,
      validParts: ValidParts
  ): Seq[PartNumber] =
    import gear.x, gear.y
    for part <- validParts.parts if part.isAdjacentTo(gear)
    yield part

def validGears(using schematic: Schematic): Iterator[ValidGear] =
  given ValidParts = ValidParts(validPartNumbers.toSeq)
  for
    (line, y) <- schematic.grid.iterator.zipWithIndex
    x <- line.indices
    gear = Gear(x, y) if schematic.charAt(x, y) == '*'
    adjacent = gear.adjacentParts if adjacent.size == 2
  yield ValidGear(gear, adjacent)
