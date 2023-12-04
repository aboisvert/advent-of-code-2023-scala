package day04

import scala.collection.mutable

@main def part2 =
  val instances = CardInstances()
  val totalCards = scala.io.Source
    .fromFile("day04/input.txt")
    .getLines
    .map(parseScratchcard)
    .map: card =>
      println(s"card ${card.id} instances ${instances(card.id)} winningCards ${card.winningCards} ")
      for nextCard <- (card.id + 1) to (card.id + card.winningCards) do
        instances.increment(nextCard, instances(card.id))
      card
    .map: card =>
      instances(card.id)
    .sum
  println(totalCards)

class CardInstances:
  private val instancesMap = mutable.Map[Int, Int]()
  def apply(cardId: Int) = instancesMap.getOrElse(cardId, 1)
  def increment(cardId: Int, x: Int) = instancesMap(cardId) = apply(cardId) + x
end CardInstances

extension (c: Scratchcard)
  def winningCards = c.mine.count(c.winning)