package org.jro.adventofcode.y2019

import org.jro.adventofcode.y2019.Day4.DupCount
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyFreeSpec with Matchers {
	"containsPair when" - {
		"number contains at least one pair should" - {
			"return true" in {
				Day4.containsPair(123456887) shouldBe true
				Day4.containsPair(22) shouldBe true
				Day4.containsPair(3223) shouldBe true
				Day4.containsPair(344) shouldBe true
				Day4.containsPair(443) shouldBe true
				Day4.containsPair(112233) shouldBe true
			}
		}
		"number contains no pair should" - {
			"return false" in {
				Day4.containsPair(1234) shouldBe false
				Day4.containsPair(1213243) shouldBe false
				Day4.containsPair(12121313) shouldBe false
				Day4.containsPair(4321) shouldBe false
			}
		}
	}

	"hasIncreasingDigits when" - {
		"number contains increasing digits should" - {
			"return true" in {
				Day4.hasIncreasingDigits(12345) shouldBe true
				Day4.hasIncreasingDigits(1223445) shouldBe true
				Day4.hasIncreasingDigits(3579) shouldBe true
				Day4.hasIncreasingDigits(35779) shouldBe true
				Day4.hasIncreasingDigits(89) shouldBe true
				Day4.hasIncreasingDigits(9) shouldBe true
			}
		}
		"number contains at least one non-increasing digits should" - {
			"return false" in {
				Day4.hasIncreasingDigits(12343) shouldBe false
				Day4.hasIncreasingDigits(12232445) shouldBe false
				Day4.hasIncreasingDigits(35709) shouldBe false
				Day4.hasIncreasingDigits(325779) shouldBe false
				Day4.hasIncreasingDigits(890) shouldBe false
				Day4.hasIncreasingDigits(98) shouldBe false
			}
		}
	}

	"countDups when" - {
		"counting dups of number with consecutive digits should" - {
			"return more than one inRules dup count" in {
				Day4.countDups(12345) should contain theSameElementsInOrderAs Seq(DupCount('1', 1), DupCount('2', 1), DupCount('3', 1), DupCount('4', 1), DupCount('5', 1))
				Day4.countDups(1123334)  should contain theSameElementsInOrderAs Seq(DupCount('1', 2), DupCount('2', 1), DupCount('3', 3), DupCount('4', 1))
				Day4.countDups(12222334)  should contain theSameElementsInOrderAs Seq(DupCount('1', 1), DupCount('2', 4), DupCount('3', 2), DupCount('4', 1))
			}
		}
	}
}
