package org.jro.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author joe_mojo.
 *         2024/12/10
 */
class Day3Spec extends AnyFreeSpec with Matchers {
	"Day3" - {
		"puzzle1" - {
			"with sample input" - {
				val sampleInputD3 = """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
				"should give 6" in {
					Day3.puzzle1(sampleInputD3) should be(161)
				}
			}
		}
		"puzzle2" - {
			"with sample input" - {
				val sampleInputD3 = """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""
				"should give 48" in {
					import scala.concurrent.ExecutionContext.Implicits.global
					Day3.puzzle2(sampleInputD3) should be(48)
				}
			}
		}
	}
}
