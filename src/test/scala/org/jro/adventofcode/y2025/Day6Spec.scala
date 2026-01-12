package org.jro.adventofcode.y2025

import org.jro.adventofcode.*
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Day6Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks with EitherValues {

	"Day6" - {
		"Problem" - {
			"should apply operators correctly" in {
				Day6.Problem(List(123, 45, 6), Day6.Operator.Multiply).solve shouldBe 33210L
				Day6.Problem(List(328, 64, 98), Day6.Operator.Add).solve shouldBe 490L

			}
		}
		"with sample data" - {
			"puzzle 1 should return 4277556" in {
				val lines = getTestInputData(2025, 6, "puzzle1").map(_.source.getLines()).value
				Day6.puzzle1(lines) shouldBe 4277556L
			}
		}
	}
}
