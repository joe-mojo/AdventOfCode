package org.jro.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author joe_mojo.
 *         2024/12/10
 */
class Day4Spec extends AnyFreeSpec with Matchers {
	"Day4" - {
		"Text" - {
			val sampleInputText = Day4.Text(
				""".M.S......
				  |..A..MSMS.
				  |.M.S.MAA..
				  |..A.ASMSM.
				  |.M.S.M....
				  |..........
				  |S.S.S.S.S.
				  |.A.A.A.A..
				  |M.M.M.M.M.
				  |..........""".stripMargin.split("\n").toIndexedSeq
			)

			"should flatten areas" in {
				val text = """abcde
							 |fghij
							 |klmno
							 |pqrst
							 |uvwxy
							 |""".stripMargin.split("\n").toIndexedSeq
				Day4.Text(text).flattenSquareArea(0,0,2) should be("abfg")
				Day4.Text(text).flattenSquareArea(0,0,3) should be("abcfghklm")
				Day4.Text(text).flattenSquareArea(1,2,3) should be("hijmnorst")
			}
			"should match patterns" in {
				val MasMasRegex = """M.S.A.M.S""".r
				MasMasRegex.matches(sampleInputText.flattenSquareArea(0, 1, 3)) should be(true)
			}
			"should count matches" in {
				sampleInputText.countMatches(Day4.SquarePattern("""M.S.A.M.S""".r, 3)) should be(2)
				sampleInputText.countMatches(Day4.SquarePattern("""M.M.A.S.S""".r, 3)) should be(1)
				sampleInputText.countMatches(Day4.SquarePattern("""S.M.A.S.M""".r, 3)) should be(1)
				sampleInputText.countMatches(Day4.SquarePattern("""S.S.A.M.M""".r, 3)) should be(5)
			}
		}
	}
}
