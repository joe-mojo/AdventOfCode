package org.jro.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day1Spec extends AnyFreeSpec with Matchers {
	"Day1" - {
		val sampleInputD1 =
			"""3   4
			  |4   3
			  |2   5
			  |1   3
			  |3   9
			  |3   3
			  |""".stripMargin
		"puzzle2 with sample input" - {
			"should give 31" in {
				val lines = sampleInputD1.split("\n").iterator
				Day1.puzzle2(lines) should be(31)
			}
		}

		"LocationCount" - {
			"should be similar to another LocationCount with same id" in {
				val lc1 = Day1.LocationCount(1, 2)
				val lc2 = Day1.LocationCount(1, 3)
				lc1.isSimilarTo(lc2) should be(Some(Day1.LocationSimilarity(lc1, 3)))
			}
			"should not be similar to another LocationCount with different id" in {
				val lc1 = Day1.LocationCount(1, 2)
				val lc2 = Day1.LocationCount(2, 3)
				lc1.isSimilarTo(lc2) should be(None)
			}
		}
	}
}
