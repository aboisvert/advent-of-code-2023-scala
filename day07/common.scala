package day07

enum HandType:
  case FiveOfKind, FourOfKind, FullHouse, ThreeOfKind, TwoPairs, OnePair,
    HighCard

def handType(cards: Seq[Char]): HandType =
  import HandType.*
  // group, count and sort similar cards together,
  // e.g., 44TT4 => Seq(card='4' -> count=3, card='T' -> count=2)
  val cardCount: Seq[(Char, Int)] =
    cards.groupBy(identity).mapValues(_.size).toSeq.sortBy(-_._2)
  if cardCount(0)._2 == 5 then FiveOfKind
  else if cardCount(0)._2 == 4 then FourOfKind
  else if cardCount(0)._2 == 3 && cardCount(1)._2 == 2 then FullHouse
  else if cardCount(0)._2 == 3 then ThreeOfKind
  else if cardCount(0)._2 == 2 && cardCount(1)._2 == 2 then TwoPairs
  else if cardCount(0)._2 == 2 then OnePair
  else HighCard
end handType

def handOrdering(
    handType: Seq[Char] => HandType,
    cardOrdering: Ordering[Char]
): Ordering[Hand] = new Ordering[Hand]:
  override def compare(h1: Hand, h2: Hand): Int =
    val ht1 = handType(h1.cards).ordinal
    val ht2 = handType(h2.cards).ordinal
    if ht1 < ht2 then 1
    else if ht1 > ht2 then -1
    else
      (h1.cards.iterator zip h2.cards.iterator)
        .map { (c1, c2) => -cardOrdering.compare(c1, c2) }
        .find(_ != 0)
        .getOrElse(0)

case class Hand(cards: Seq[Char], bid: Long):
  def winnings(using hands: HandSet): Long = bid * hands.ranks(this)
end Hand

case class HandSet(hands: Seq[Hand], ordering: Ordering[Hand]):
  // assign a rank (1, 2, 3, ...) based on strength of cards
  lazy val ranks: Map[Hand, Int] =
    hands
      .sorted(ordering)
      .zipWithIndex
      .map { (c, i) => (c, i + 1) }
      .toMap
end HandSet

def parseHands(lines: Iterator[String]): Seq[Hand] =
  lines.map(parseHand).to(Seq)

def parseHand(line: String): Hand =
  val Array(cardsStr, bidStr) = line.split(" ")
  Hand(cardsStr.toSeq, bidStr.toLong)

def printTotalWinnings()(using ordering: Ordering[Hand]) =
  val lines = scala.io.Source
    .fromFile("day07/input.txt")
    .getLines
  val hands = parseHands(lines)

  given handSet: HandSet = HandSet(hands, ordering)

  println("ranks")
  handSet.ranks.toSeq
    .sortBy(_._2)
    .foreach: (hand, rank) =>
      println((hand, rank, handType(hand.cards)))

  val total = handSet.hands
    .map(_.winnings)
    .sum
  println(total)
end printTotalWinnings
