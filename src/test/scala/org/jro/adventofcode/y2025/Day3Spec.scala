package org.jro.adventofcode.y2025

import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table

/**
 * @author joe_mojo.
 *         2025/12/03
 */
class Day3Spec extends AnyFreeSpec with Matchers with Inside with TableDrivenPropertyChecks {
	"Day3" - {
		val sampleData = """987654321111111
						   |811111111111119
						   |234234234234278
						   |818181911112111
						   |""".stripMargin.split("\n").map(Day3.BatteryBank.unsafeParse).toSeq

		"combinedJoltage" - {
			"should return correct combined joltage" in {
				val bank = Day3.BatteryBank(IndexedSeq(1, 2, 3, 4, 5))
				bank.combinedJoltage(0, 1) should be(12)
				bank.combinedJoltage(1, 3) should be(24)
				bank.combinedJoltage(2, 4) should be(35)
			}
		}

		val maxCombinedJoltageTable = Table(
			("joltages", "expectedMaxCombinedJoltage"),
			(sampleData(0), 98),
			(sampleData(1), 89),
			(sampleData(2), 78),
			(sampleData(3), 92)
		)
		"max combinedJoltage iterator" - {
			"should return correct max combined joltage" in {
				forEvery(maxCombinedJoltageTable) { (joltages, expectedMaxCombinedJoltage) =>
					joltages.combined2JoltagesIterator.max should be(expectedMaxCombinedJoltage)
				}
			}
		}

		"puzzle1 with sample input" - {
			"should give 357" in {
				Day3.puzzle1(sampleData) should be(357L)
			}
		}

		"combinedJoltage with multiple indices" - {
			"should return correct combined joltage" in {
				//                                     0  1  2  3  4  5  6
				val bank = Day3.BatteryBank(IndexedSeq(1, 2, 3, 4, 5, 8, 8))
				bank.combinedJoltage(IndexedSeq(0, 1, 2)) should be(123L)
				bank.combinedJoltage(IndexedSeq(1, 3, 4)) should be(245L)
				bank.combinedJoltage(IndexedSeq(2, 4)) should be(35L)
				bank.combinedJoltage(IndexedSeq(1,3,5,6)) should be(2488L)

			}
		}

		"puzzle2 with sample input" - {
			"should give 3121910778619" in {
				Day3.puzzle2(sampleData) should be(3121910778619L)
			}
		}
	}
}
