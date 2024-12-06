package org.jro.adventofcode.y2020

import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day9Spec extends AnyFreeSpec with Matchers with Inside {

	private val previousNumberSample1to25: Seq[Long] = 1L to 25L
	private val previousNumberSample20_1to25_45: Seq[Long] = 20L +: (1L to 19L) ++: (21L to 25L) ++: Seq(45L)

	private val sampleForLast5: Seq[Long] = """35
													|20
													|15
													|25
													|47
													|40
													|62
													|55
													|65
													|95
													|102
													|117
													|150
													|182
													|127
													|219
													|299
													|277
													|309
													|576""".stripMargin.split("""\n""").toIndexedSeq.map(_.toLong)

	"Validity check for last 25 numbers should" - {
		"validate acceptable numbers" in {
			Day9.isValidFor25(26, previousNumberSample1to25) shouldBe true
			Day9.isValidFor25(49, previousNumberSample1to25) shouldBe true
			Day9.isValidFor25(26, previousNumberSample20_1to25_45) shouldBe true
			Day9.isValidFor25(64, previousNumberSample20_1to25_45) shouldBe true
			Day9.isValidFor25(66, previousNumberSample20_1to25_45) shouldBe true

		}
		"invalidate unacceptable numbers" in {
			Day9.isValidFor25(100, previousNumberSample1to25) shouldBe false
			Day9.isValidFor25(50, previousNumberSample1to25) shouldBe false
			Day9.isValidFor25(65, previousNumberSample20_1to25_45) shouldBe false
		}

	}
	"Validity check for last 5 numbers should" - {
		"validate all except 127" in {
			sampleForLast5.sliding(6).map { numbers =>
				info(s"testing $numbers")
				val actualResult = Day9.isValid(numbers.last, numbers.take(5), 5)
				if(numbers.last == 127) {
					actualResult shouldBe false
				} else actualResult shouldBe true
			}
		}
	}

	"findSumOfSuites should" - {
		"find the suite 15, 25, 47, 40 when the target sum is 127" in {
			val actualResult = Day9.findSumOfSuites(127, sampleForLast5)
			println(s"actualResult: $actualResult")
			actualResult should contain theSameElementsInOrderAs Seq(Seq(15L, 25L, 47L, 40L))
		}
	}

}
