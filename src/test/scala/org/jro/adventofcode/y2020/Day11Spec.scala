package org.jro.adventofcode.y2020

import org.jro.adventofcode.Error
import org.jro.adventofcode.y2020.Day11.{RoundIterator, SeatMap}
import org.jro.adventofcode.y2020.Day11.Status.{EmptySeat, Floor, OccupiedSeat}
import org.scalatest.{EitherValues, Inside}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFreeSpec with Matchers with  Inside with EitherValues {
	val smallSample = """L.LL.LL.LL
						|LLLLLLL.LL
						|L.L.L..L..
						|LLLL.LL.LL
						|L.LL.LL.LL
						|L.LLLLL.LL
						|..L.L.....
						|LLLLLLLLLL
						|L.LLLLLL.L
						|L.LLLLL.LL""".stripMargin

	val smallSampleR2 = """#.##.##.##
						  |#######.##
						  |#.#.#..#..
						  |####.##.##
						  |#.##.##.##
						  |#.#####.##
						  |..#.#.....
						  |##########
						  |#.######.#
						  |#.#####.##""".stripMargin

	val smallSampleR3 = """#.LL.L#.##
						  |#LLLLLL.L#
						  |L.L.L..L..
						  |#LLL.LL.L#
						  |#.LL.LL.LL
						  |#.LLLL#.##
						  |..L.L.....
						  |#LLLLLLLL#
						  |#.LLLLLL.L
						  |#.#LLLL.##""".stripMargin

	val smallSampleR4 = """#.##.L#.##
						  |#L###LL.L#
						  |L.#.#..#..
						  |#L##.##.L#
						  |#.##.LL.LL
						  |#.###L#.##
						  |..#.#.....
						  |#L######L#
						  |#.LL###L.L
						  |#.#L###.##""".stripMargin

	val smallSampleR5 = """#.#L.L#.##
						  |#LLL#LL.L#
						  |L.L.L..#..
						  |#LLL.##.L#
						  |#.LL.LL.LL
						  |#.LL#L#.##
						  |..L.L.....
						  |#L#LLLL#L#
						  |#.LLLLLL.L
						  |#.#L#L#.##""".stripMargin

	val smallSampleR6 = """#.#L.L#.##
						  |#LLL#LL.L#
						  |L.#.L..#..
						  |#L##.##.L#
						  |#.#L.LL.LL
						  |#.#L#L#.##
						  |..L.L.....
						  |#L#L##L#L#
						  |#.LLLLLL.L
						  |#.#L#L#.##""".stripMargin

	"SeatMap should" - {
		"parse correctly input when" - {
			"fed with smallSample" in {
				inside(SeatMap.ofLines(smallSample.split("\n").iterator)){
					case Right(sm@SeatMap(_)) => sm.show shouldBe smallSample
				}
			}
		}

		"find ajdacent indices when" - {
			"build from smallSample" in {
				val sm: SeatMap = SeatMap.ofLines(smallSample.split("\n").iterator).right.value
				sm.adjacentIndices(0, 0) should contain theSameElementsAs Seq((1, 0), (1,1), (0,1))
				sm.adjacentIndices(1, 0) should contain theSameElementsAs Seq(
					(2, 0), (2,1), (1,1), (0, 1), (0, 0)
				)
				sm.adjacentIndices(9, 9) should contain theSameElementsAs Seq(
					(8, 8), (9, 8), (8, 9)
				)

			}
		}

		"count adjacent occupied seats when" - {
			"build from smallSample" in {
				val sm: SeatMap = SeatMap.ofLines(smallSample.split("\n").iterator).right.value
				sm.adjacentStatuses(0, 0) should contain theSameElementsAs Seq(
					Floor, EmptySeat, EmptySeat
				)
				sm.adjacentOccupiedStatusCount(0, 0) shouldBe 0
				sm.adjacentOccupiedStatusCount(1, 0) shouldBe 0
			}
			"build from smallSampleR4" in {
				val sm: SeatMap = SeatMap.ofLines(smallSampleR4.split("\n").iterator).right.value
				sm.adjacentStatuses(0, 0) should contain theSameElementsAs Seq(
					Floor, EmptySeat, OccupiedSeat
				)
				sm.adjacentOccupiedStatusCount(0, 0) shouldBe 1
			}
		}

		"compute next state" in {
			val sm: SeatMap = SeatMap.ofLines(smallSample.split("\n").iterator).right.value

			Seq(smallSampleR2, smallSampleR3, smallSampleR4, smallSampleR5, smallSampleR6).zipWithIndex.
				foldLeft(sm) { (formerMap, newSampleAndIndex) =>
					info(s"Round ${newSampleAndIndex._2}")
					val newMap = formerMap.nextSeatMap
					newMap.show shouldBe newSampleAndIndex._1
					newMap
				}
		}

		"stop changing" - {
			"at step 37 when starting with smallSample" in {
				val sm: SeatMap = SeatMap.ofLines(smallSample.split("\n").iterator).right.value

				val rounds: LazyList[(SeatMap, Int)] = LazyList.iterate((sm, 0)){
					case (currentSeatMap, index) => (currentSeatMap.nextSeatMap, index + 1)
				}

				rounds(5)._1 shouldBe rounds(6)._1

				val roundIterator = RoundIterator(sm)

				while(!roundIterator.playNextRound())()

				roundIterator.lastRound shouldBe 6

			}
		}
	}


}
