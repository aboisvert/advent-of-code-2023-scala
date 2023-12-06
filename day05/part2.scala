package day05.part2

import day05.part1.{Input, parseInput, Conversion, UnitConversion}

type Range = scala.collection.immutable.NumericRange[Long]

@main def part2 =
  val Input(seedRanges, unitConversions) = parseInput(
    scala.io.Source
      .fromFile("day05/input.txt")
      .getLines
  )

  val locations =
    for seedRange <- seedRanges.grouped(2)
    yield
      val Array(rangeStart, rangeLength) = seedRange.to(Array)
      val seedRange2 = rangeStart until (rangeStart + rangeLength)
      unitConversions.foldLeft(Iterable(seedRange2: Range)): (ranges, uc) =>
        uc.applyRanges(ranges)
    end for
  println(locations.flatten.map(_.start).min)
end part2

extension (uc: UnitConversion)
  def applyRanges(ranges: Iterable[Range]): Iterable[Range] =
    println(s"apply ${uc.fromUnit} -> ${uc.toUnit}")
    val result = ranges.flatMap { range => uc.applyRange(range) }
    println(s"ranges result = $result")
    println()
    result

  def applyRange(range: Range): Iterable[Range] =
    println(s"apply ${uc.fromUnit} -> ${uc.toUnit} to $range")
    val result = uc.conversions.foldLeft(Result(pending = Iterable(range))):
      (result, c) =>
        val results = result.pending.map(range => c.applyRange(range))
        Result(
          pending = results.map(_.pending).flatten,
          converted = result.converted ++ results.map(_.converted).flatten
        )
    println(s"range = $result")
    result.pending ++ result.converted

case class Result(
    pending: Iterable[Range] = Iterable.empty,
    converted: Iterable[Range] = Iterable.empty
)

extension (c: Conversion)
  def applyRange(range: Range): Result =
    import c.*

    val sourceRangeEnd = sourceRangeStart + rangeLength - 1

    // range below
    if range.end < sourceRangeStart then
      return Result(pending = Iterable(range))

    // range above
    if range.start > sourceRangeEnd then
      return Result(pending = Iterable(range))

    // range overlap left
    if range.start < sourceRangeStart && //
      range.end >= sourceRangeStart && //
      range.end <= sourceRangeEnd
    then
      return Result(
        pending = Iterable(range.start until sourceRangeStart),
        converted = Iterable(c(sourceRangeStart) to c(range.end))
      )

    // range overlap right
    if range.start >= sourceRangeStart && range.end > sourceRangeEnd then
      return Result(
        converted = Iterable(c(range.start) to c(sourceRangeEnd)),
        pending = Iterable((sourceRangeEnd + 1) to range.end)
      )

    // within
    if range.start >= sourceRangeStart && range.end <= sourceRangeEnd then
      return Result(converted = Iterable(c(range.start) to c(range.end)))

    // full overlap
    return Result(
      pending = Iterable(
        range.start until sourceRangeStart,
        (sourceRangeEnd + 1) to range.end
      ),
      converted = Iterable(c(sourceRangeStart) to c(sourceRangeEnd))
    )
  end applyRange
