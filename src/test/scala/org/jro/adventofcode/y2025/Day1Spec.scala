package org.jro.adventofcode.y2025

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author joe_mojo.
 *         2025/12/01
 */
class Day1Spec extends AnyFreeSpec with Matchers {
	"Day1" - {
		val sampleInputD1 =
			"""L68
			  |L30
			  |R48
			  |L5
			  |R60
			  |L55
			  |L1
			  |L99
			  |R14
			  |L82
			  |""".stripMargin.split("\n").iterator.toSeq
		"puzzle1 with sample input" - {
			"should give 3" in {
				Day1.puzzle1(sampleInputD1.iterator) should be(3)
			}
		}
		"puzzle2 with sample input" - {
			"should give 0" in {
				Day1.puzzle2(sampleInputD1.iterator) should be(6)
			}
		}
	}
}
