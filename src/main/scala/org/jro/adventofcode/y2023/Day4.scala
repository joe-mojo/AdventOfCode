package org.jro.adventofcode.y2023

import org.jro.adventofcode
import org.jro.adventofcode.getInputLinesOrThrow

import scala.collection.immutable.SortedMap
import scala.collection.mutable.{SortedMap => MutSortedMap}
import scala.util.matching.Regex

object Day4 {
	case class CardCopies(card: Card, instances: Int = 1) {
		def inc: CardCopies = CardCopies(card, instances + 1)
	}

	case class Card(id: Int, winning: Set[Int], owned: Set[Int]) {
		def score: Int = {
			Math.pow(2, matchCount - 1).toInt
		}

		def matchCount: Int = winning.intersect(owned).size
	}
	object Card {
		val CardRegex: Regex = """^Card\s+(\d+):([ 0-9]+)\|([ 0-9]+)$""".r
		def parseNumbers(linePart: String): Set[Int] =
			linePart.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt).toSet

		def parse(line: String): Card = line match
			case CardRegex(cid, winStr, ownedStr) => Card(cid.toInt, parseNumbers(winStr), parseNumbers(ownedStr))
	}

	def puzzle1(lines: Iterable[String]): Int = {
		lines.map(Card.parse).map(_.score).sum
	}

	def puzzle2(lines: Iterable[String]): Int = {
		val cards: MutSortedMap[Int, CardCopies] = MutSortedMap() ++ (lines.map { line =>
			val card = Card.parse(line)
			card.id -> CardCopies(card)
		})
		cards.keys.foreach { cid =>
			val scoringCard = cards(cid)
			for(i <- (cid + 1) to (cid + scoringCard.card.matchCount)) {
				val copies = cards(i)
				cards.update(i, copies.copy(instances = copies.instances + scoringCard.instances))
			}
		}
		cards.values.map(_.instances).sum
	}

	private val Sample =
		"""Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
		  |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
		  |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
		  |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
		  |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
		  |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin.split("\\n")

	def main(args: Array[String]): Unit = {
		val inputData = getInputLinesOrThrow(2023, 4)
		println(s"Puzzle1 with sample = ${puzzle1(Sample)}")
		println(s"Puzzle1 = ${puzzle1(inputData)}")

		println(s"Puzzle2 with sample = ${puzzle2(Sample)}")
		println(s"Puzzle2 = ${puzzle2(inputData)}")
	}

}
