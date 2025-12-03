package org.jro.adventofcode.y2025

import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author joe_mojo.
 *         2025/12/02
 */
class Day2Spec extends AnyFreeSpec with Matchers with Inside {
	"Day2" - {
		val sampleData = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124\n"

		"puzzle1 with sample input" - {
			"should give 1227775554" in {
				val ranges = inside(Day2.parseInput(Seq(sampleData).iterator)) {
					case Right(ranges) => ranges
					case Left(err) => fail(s"Parsing failed with error: $err")
				}
				Day2.puzzle1(ranges) should be(1227775554L)
			}
		}

		"isValidId" - {
			"should return true for valid ids" in {
				Day2.Part1.isValidId(123456L) should be(true)
				Day2.Part1.isValidId(1L) should be(true)
				Day2.Part1.isValidId(12L) should be(true)
				Day2.Part1.isValidId(101L) should be(true)
				Day2.Part1.isValidId(123L) should be(true)
			}
			"should return false for invalid ids" in {
				Day2.Part1.isValidId(1212L) should be(false)
				Day2.Part1.isValidId(123123L) should be(false)
				Day2.Part1.isValidId(12341234L) should be(false)
				Day2.Part1.isValidId(999999L) should be(false)
				Day2.Part1.isValidId(12121212L) should be(false)
			}
		}
	}
}
