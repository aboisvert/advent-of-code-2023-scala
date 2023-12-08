package day07.part2

import day07.*

@main def part2 =
  given Ordering[Hand] =
    handOrdering(handTypeWithJokers, cardOrderingWithJokers)
  printTotalWinnings()

def handTypeWithJokers(cards: Seq[Char]): HandType =
  import HandType.*

  val jokers = cards.count(_ == 'J')
  if jokers == 5 then return FiveOfKind

  // group, count and sort similar cards together,
  // e.g., 44TT4 => Seq(card='4' -> count=3, card='T' -> count=2)
  val cardCount: Seq[(Char, Int)] =
    cards
      .filterNot(_ == 'J')
      .groupBy(identity)
      .mapValues(_.size)
      .toSeq
      .sortBy(-_._2)

  val newHand =
    cardCount.updated(0, cardCount(0).copy(_2 = cardCount(0)._2 + jokers))

  handType(
    newHand.foldLeft(""): (cardsStr, cardCount) =>
      cardsStr + (cardCount._1.toString * cardCount._2)
  )

val cardOrderingWithJokers = new Ordering[Char]:
  val cardsOrder = "AKQT98765432J".toSeq
  override def compare(c1: Char, c2: Char): Int =
    cardsOrder.indexOf(c1) compare cardsOrder.indexOf(c2)
