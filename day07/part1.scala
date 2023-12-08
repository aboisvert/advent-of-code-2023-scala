package day07.part1

import day07.*

@main def part1 =
  given Ordering[Hand] = handOrdering(handType, cardOrdering)
  printTotalWinnings()

val cardOrdering = new Ordering[Char]:
  val cardsOrder = "AKQJT98765432".toSeq
  override def compare(c1: Char, c2: Char): Int =
    cardsOrder.indexOf(c1) compare cardsOrder.indexOf(c2)
