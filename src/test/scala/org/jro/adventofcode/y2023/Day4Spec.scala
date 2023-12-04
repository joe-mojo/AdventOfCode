package org.jro.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFreeSpec with Matchers{
	"Day4 Card winning count" in {
		val card = Day4.Card.parse("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
		card.score should be(8)
	}
}
