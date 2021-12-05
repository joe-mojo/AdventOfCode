package org.jro.adventofcode.y2021

import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author joe_mojo.
 *         2021/12/05
 */
class Day5Spec extends AnyFreeSpec with Matchers with Inside {
	val sampleInput = """0,9 -> 5,9
						|8,0 -> 0,8
						|9,4 -> 3,4
						|2,2 -> 2,1
						|7,0 -> 7,4
						|6,4 -> 2,0
						|0,9 -> 2,9
						|3,4 -> 1,4
						|0,0 -> 8,8
						|5,5 -> 8,2""".stripMargin
	val sampleInputExpectedRes = 5
	val expectedSegments = Seq(
		(Point(0,9),Point(5,9)),
		(Point(8,0),Point(0,8)),
		(Point(9,4),Point(3,4)),
		(Point(2,2),Point(2,1)),
		(Point(7,0),Point(7,4)),
		(Point(6,4),Point(2,0)),
		(Point(0,9),Point(2,9)),
		(Point(3,4),Point(1,4)),
		(Point(0,0),Point(8,8)),
		(Point(5,5),Point(8,2))
	)


	"parseLine" - {
		"should parse line in a pair of points" in {
			val segments: Seq[(Point, Point)] = sampleInput.split("\n").map(Day5.parseLine).toSeq
			segments should contain theSameElementsInOrderAs expectedSegments
		}
	}
	"expandHV" - {
		"should expand horizontal line" in {
			val expandedSegment = Day5.expandHV((Point(9,4),Point(3,4)))
			expandedSegment should contain theSameElementsAs Seq(
				Point(9,4), Point(8,4), Point(7,4), Point(6,4), Point(5,4), Point(4,4), Point(3,4)
			)
		}
	}
	"expandHVD" - {
		"should expand diagonal line" in {
			val expandedSegment = Day5.expandHVD(Point(8,0) -> Point(0,8))
			expandedSegment should contain theSameElementsInOrderAs Seq(
				Point(8,0), Point(7,1), Point(6,2), Point(5,3), Point(4,4), Point(3, 5), Point(2, 6), Point(1, 7), Point(0, 8)
			)
		}
	}
	"should expand vertical line" in {
		val expandedSegment = Day5.expandHVD(Point(9,4) -> Point(3,4))
		expandedSegment should contain theSameElementsInOrderAs Seq(
			Point(9,4), Point(8,4), Point(7,4), Point(6,4), Point(5,4), Point(4, 4), Point(3, 4)
		)
	}

}
