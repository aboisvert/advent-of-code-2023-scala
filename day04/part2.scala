package day04

@main def part2 =
  case class Acc(sum: Int, instances: CardInstances)
  val result = scala.io.Source
    .fromFile("day04/input.txt")
    .getLines
    .map(parseScratchcard)
    .foldLeft(Acc(0, CardInstances())): (acc, card) =>
      import acc.{sum, instances}
      println(s"card ${card.id} instances ${instances(card.id)} winningCards ${card.winningCards} ")
      for nextCard <- (card.id + 1) to (card.id + card.winningCards) do
        instances.increment(nextCard, instances(card.id))
      Acc(sum + instances.dropAndGetValue(card.id), instances)
  println(result.sum)

class CardInstances:
  private val instancesMap = scala.collection.mutable.Map[Int, Int]()
  def apply(cardId: Int) = instancesMap.getOrElse(cardId, 1)
  def increment(cardId: Int, x: Int) = instancesMap(cardId) = apply(cardId) + x
  def dropAndGetValue(cardId: Int) = instancesMap.remove(cardId).getOrElse(1)
  def sum = instancesMap.values.sum
end CardInstances

extension (c: Scratchcard)
  def winningCards = c.mine.count(c.winning)