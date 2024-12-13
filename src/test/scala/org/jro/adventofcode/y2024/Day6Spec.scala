package org.jro.adventofcode.y2024

import org.jro.adventofcode.y2024.Day6.{Location, Room, getObstacles}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author joe_mojo.
 *         2024/12/14
 */
class Day6Spec extends AnyFreeSpec with Matchers {
	"Day6" - {
		val sampleInput: IndexedSeq[String] =
			"""....#.....
			  |.........#
			  |..........
			  |..#.......
			  |.......#..
			  |..........
			  |.#..^.....
			  |........#.
			  |#.........
			  |......#...""".stripMargin.split("\n").toIndexedSeq
		val room = Room(getObstacles(sampleInput), sampleInput.size)
		val start = Day6.findStartLocation(sampleInput, room)
		"getObstacles" - {
			"should return the obstacles" in {
				getObstacles(sampleInput) should contain theSameElementsAs(Set(
					Location(4, 0),
					Location(9, 1),
					Location(2, 3),
					Location(7, 4),
					Location(1, 6),
					Location(8, 7),
					Location(0, 8),
					Location(6, 9)
				))
			}
		}
		"puzzle1" - {
			"should return the number of obstacles" in {
				Day6.puzzle1(room, start) shouldBe 41
			}
		}
		"puzzle2" - {
			"should return the number of obstacles" in {
				Day6.puzzle2(room, start) shouldBe 6
			}
		}
	}
}
