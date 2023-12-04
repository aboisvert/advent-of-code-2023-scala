package day04

@main def part2 =
  /** Accumulator */
  case class Acc(sum: Int, instances: CardInstances)

  val result = scala.io.Source
    .fromFile("day04/input.txt")
    .getLines
    .map(parseScratchcard)
    .foldLeft(Acc(0, new CardInstances)): (acc, card) =>
      import acc.{sum, instances}

      // Increment count of instances of following cards based on `winningCards` */
      for nextCard <- (card.id + 1) to (card.id + card.winningCards) do
        instances.increment(nextCard, instances(card.id))

      // Carry over the accumulator for `sum` and card `instances`
      Acc(sum + instances.getValueAndDiscard(card.id), instances)

  println(result.sum)

class CardInstances:
  /** CardId -> count of instances */
  private val instancesMap = scala.collection.mutable.Map[Int, Int]()

  /** Get count of instances for `cardId`, with a default of 1 */
  def apply(cardId: Int) = instancesMap.getOrElse(cardId, 1)

  /** Increment the count of `cardId` */
  def increment(cardId: Int, inc: Int) = instancesMap(cardId) =
    apply(cardId) + inc

  /** Get the value of `cardId` then discard its value. */
  def getValueAndDiscard(cardId: Int) = instancesMap.remove(cardId).getOrElse(1)

end CardInstances

extension (c: Scratchcard)
  /** Returns the count of winningCards */
  def winningCards = c.myNumbers.count(c.winningNumbers)
