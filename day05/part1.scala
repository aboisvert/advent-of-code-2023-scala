package day05.part1

@main def part1 =
  val lines = scala.io.Source
    .fromFile("day05/input.txt")
    .getLines

  val Input(seeds, unitConversions) = parseInput(lines)

  val locations =
    for seed <- seeds
    yield unitConversions.foldLeft(seed) { (num, uc) => uc(num) }

  println(locations.min)
end part1

case class Input(seeds: Iterable[Long], conversions: Iterable[UnitConversion])

case class Conversion(
    destinationRangeStart: Long,
    sourceRangeStart: Long,
    rangeLength: Long
):
  def isDefinedAt(source: Long) = sourceRange.contains(source)

  def sourceRange = sourceRangeStart until (sourceRangeStart + rangeLength)

  /** Converts `sourceNumber` into the destination range if it's within the
    * sourceRange, or returns the sourceNumber as-is otherwise.
    */
  def apply(sourceNumber: Long): Long =
    if sourceRange.contains(sourceNumber) then
      (sourceNumber - sourceRangeStart + destinationRangeStart)
    else sourceNumber

  override def toString =
    val sourceRangeEnd = sourceRangeStart + rangeLength - 1
    val destinationRangeEnd = destinationRangeStart + rangeLength - 1
    s"Conversion($sourceRangeStart-$sourceRangeEnd => $destinationRangeStart-$destinationRangeEnd)"

case class UnitConversion(
    fromUnit: String,
    toUnit: String,
    conversions: Iterable[Conversion]
):
  def apply(sourceNumber: Long): Long =
    conversions
      .find(c => c.isDefinedAt(sourceNumber))
      .map(c => c(sourceNumber))
      .getOrElse(sourceNumber)

def parseInput(lines: Iterator[String]): Input =
  // parse seeds: "seeds: 79 14 55 13"
  val seeds = lines.next().split(": ") match
    case Array(seedsPart, numbersPart) =>
      numbersPart.split(" ").map(_.toLong).to(Iterable)
  lines.next()

  // parse unit conversions
  var unitConversions = scala.collection.mutable.ArrayBuffer[UnitConversion]()

  val MappingRe = "(.+)-to-(.+) map:".r
  while lines.hasNext do
    // parse unit conversion

    // "soil-to-fertilizer map:""
    val (fromUnit, toUnit) = lines.next() match
      case MappingRe(fromUnit, toUnit) => (fromUnit, toUnit)

    // "0 15 37"
    // "37 52 2"
    // "39 0 15"
    val conversions = lines
      .takeWhile(_.trim != "")
      .map: line =>
        val Array(destinationRangeStart, sourceRangeStart, rangeLength) =
          line.trim.split("""\s+""").map(_.toLong)
        Conversion(destinationRangeStart, sourceRangeStart, rangeLength)

    unitConversions += UnitConversion(
      fromUnit,
      toUnit,
      conversions.to(Iterable)
    )
  end while

  Input(seeds, unitConversions)
end parseInput

@main def test1 =
  val Input(seeds, unitConversions) = parseInput(
    scala.io.Source
      .fromString("""seeds: 79 14 55 13
    |
    |seed-to-soil map:
    |50 98 2
    |52 50 48
    |
    |soil-to-fertilizer map:
    |0 15 37
    |37 52 2
    |39 0 15
    """.stripMargin)
      .getLines
  )
  println(s"seeds: " + seeds)
  assert(seeds == Iterable(79, 14, 55, 13))
  println(s"unitConversions: ")
  unitConversions foreach println
  assert(
    unitConversions.toSeq(0) == UnitConversion(
      "seed",
      "soil",
      Iterable(
        Conversion(50, 98, 2),
        Conversion(52, 50, 48)
      )
    )
  )
