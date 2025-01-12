package org.jro.adventofcode.y2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import Day10.{TopographicMap, Coord}

/**
 * @author joe_mojo.
 *         2025/01/11
 */
class Day10Spec extends AnyFreeSpec with Matchers {
	val sampleInput = """89010123
						|78121874
						|87430965
						|96549874
						|45678903
						|32019012
						|01329801
						|10456732""".stripMargin
	"parseLines" - {
		"with sampleInput" - {
			"should parse lines into topographic map" in {
				val result = Day10.parseLines(sampleInput.linesIterator)
				result shouldBe Right(TopographicMap(IndexedSeq(
					IndexedSeq(8, 9, 0, 1, 0, 1, 2, 3),
					IndexedSeq(7, 8, 1, 2, 1, 8, 7, 4),
					IndexedSeq(8, 7, 4, 3, 0, 9, 6, 5),
					IndexedSeq(9, 6, 5, 4, 9, 8, 7, 4),
					IndexedSeq(4, 5, 6, 7, 8, 9, 0, 3),
					IndexedSeq(3, 2, 0, 1, 9, 0, 1, 2),
					IndexedSeq(0, 1, 3, 2, 9, 8, 0, 1),
					IndexedSeq(1, 0, 4, 5, 6, 7, 3, 2)
				)))
			}
		}
	}

	"countAccessibleSummitsFrom" - {
		"with sampleInput" - {
			val topographicMap: TopographicMap = Day10.parseLines(sampleInput.linesIterator).getOrElse(fail())
			"starting at (2;0) should return 5" in {
				topographicMap.countAccessibleSummitsFrom(Coord(2, 0)) shouldBe 5
			}
			"using all starting points should return 36" in {
				topographicMap.findStartingPoints.map(topographicMap.countAccessibleSummitsFrom).sum shouldBe 36
			}
		}
	}

	"countRatedPathsFrom" - {
		"with sampleInput" - {
			val topographicMap: TopographicMap = Day10.parseLines(sampleInput.linesIterator).getOrElse(fail())
			"starting at (2;0) should return 20" in {
				topographicMap.countRatedPathsFrom(Coord(2, 0)) shouldBe 20
			}
			"using all starting points should return 1" in {
				topographicMap.findStartingPoints.map(topographicMap.countRatedPathsFrom).sum shouldBe 81
			}
		}
		"with sample 5x5" - {
			val sample5x5 = """012345
							  |123456
							  |234567
							  |345678
							  |406789
							  |567890""".stripMargin
			"starting at (0;0) should return 227" in {
				val topographicMap: TopographicMap = Day10.parseLines(sample5x5.linesIterator).getOrElse(fail())
				topographicMap.countRatedPathsFrom(Coord(0, 0)) shouldBe 227
			}
		}
	}
}
