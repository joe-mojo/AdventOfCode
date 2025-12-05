package org.jro.adventofcode.y2025

import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

/**
 * @author joe_mojo.
 *         2025/12/05
 */
class Day5Spec extends AnyFreeSpec with Matchers with Inside with TableDrivenPropertyChecks {
	val sampleData = """3-5
					   |10-14
					   |16-20
					   |12-18
					   |
					   |1
					   |5
					   |8
					   |11
					   |17
					   |32""".stripMargin
	"Day5" - {
		"mergeRanges" - {
			"should merge overlapping and contiguous ranges correctly" in {
				val ranges = Seq(
					26L to 30,
					1L to 5,
					4L to 10,
					30L to 35,
					12L to 15,
					14L to 20,
					22L to 25
				)
				val merged = Day5.ProductsData.mergeRanges(ranges)
				merged shouldEqual Seq(
					1 to 10,
					12 to 20,
					22 to 25,
					26 to 35
				)
			}
		}
		"with sample data" - {
			val sampleInput = {
				val lines = sampleData.split("\n").toIterator
				val rangeLines = Iterator.continually(lines.next()).takeWhile(_.nonEmpty).toSeq
				val productIdLines = lines.toSeq

				val ranges = rangeLines.map(Day5.unsafeParseRange)
				val productIds = productIdLines.map(_.toLong)

				Day5.ProductsData(ranges, productIds)
			}
			"should parse ranges and product IDs correctly" in {
				Day5.puzzle1(sampleInput) should be(3L)
			}
		}
	}
}
