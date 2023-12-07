package day07

import scala.io.Source

@main def part1 =
  val lines = scala.io.Source
    .fromFile("day07/input-verif.txt")
    .getLines

  given handSet: HandSet = parseHands(lines)

  println("ranks")
  handSet.ranks.toSeq
    .sortBy(_._2)
    .foreach: (hand, rank) =>
      println((hand, rank, hand.handType))

  val total = handSet.hands
    .map(_.winnings)
    .sum
  println(total)
end part1

enum HandType:
  case FiveOfKind, FourOfKind, FullHouse, ThreeOfKind, TwoPairs, OnePair,
    HighCard

case class Hand(cards: Seq[Char], bid: Long):
  lazy val handType =
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

  def winnings(using hands: HandSet): Long = bid * hands.ranks(this)
end Hand

val cardOrdering = new Ordering[Char]:
  val cardsOrder = "AKQJT98765432".toSeq
  override def compare(c1: Char, c2: Char): Int =
    cardsOrder.indexOf(c1) compare cardsOrder.indexOf(c2)

val hardOrdering: Ordering[Hand] = new Ordering[Hand]:
  override def compare(h1: Hand, h2: Hand): Int =
    if h1.handType.ordinal < h2.handType.ordinal then 1
    else if h1.handType.ordinal > h2.handType.ordinal then -1
    else
      (h1.cards.iterator zip h2.cards.iterator)
        .map { (c1, c2) => -cardOrdering.compare(c1, c2) }
        .find(_ != 0)
        .getOrElse(0)

case class HandSet(hands: Seq[Hand]):
  // assign a rank (1, 2, 3, ...) based on strength of cards
  lazy val ranks: Map[Hand, Int] =
    hands
      .sorted(hardOrdering)
      .zipWithIndex
      .map { (c, i) => (c, i + 1) }
      .toMap

def parseHands(lines: Iterator[String]): HandSet =
  HandSet(lines.map(parseHand).to(Seq))

def parseHand(line: String): Hand =
  val Array(cardsStr, bidStr) = line.split(" ")
  Hand(cardsStr.toSeq, bidStr.toLong)
