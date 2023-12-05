package org.jro.adventofcode.y2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author joe_mojo.
 *         2023/12/03
 */
class Day3Spec extends AnyFreeSpec with Matchers{
	val sampleData =
		"""467..114..
		  |...*......
		  |..35..633.
		  |......#...
		  |617*......
		  |.....+.58.
		  |..592.....
		  |......755.
		  |...$.*....
		  |.664.598..""".stripMargin.split("\\n").toIndexedSeq

	"Day3 puzzle 1" - {
		"with sample data" - {
			"should give 4361" in {
				Day3.puzzle1(sampleData) should be(4361)
			}
		}
	}
	"findNumbersAround" - {
		"with sample data" - {
			"should find numbers" in {
				Day3.findNumbersAround(sampleData, 1, 3) should contain theSameElementsAs(Seq(467, 35))
			}
		}
	}
}
