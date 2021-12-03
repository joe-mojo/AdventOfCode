package org.jro.adventofcode.y2020

import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day6Spec extends AnyFreeSpec with Matchers with Inside {
	val sampleInput = """abc
						|
						|a
						|b
						|c
						|
						|ab
						|ac
						|
						|a
						|a
						|a
						|a
						|
						|b""".stripMargin

	"Puzzle1" - {
		"fed with sample input" - {
			"should give 11" in {
				Day6.puzzle1(sampleInput) shouldBe 11
			}
		}
	}
	"Puzzle2" - {
		"fed with sample input" - {
			"should give 6" in {
				Day6.puzzle2(sampleInput) shouldBe 6
			}
		}
	}

}
