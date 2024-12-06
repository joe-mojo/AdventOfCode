package org.jro.adventofcode.y2020

import org.jro.adventofcode.y2020.Day12.Angle.{`180°`, `270°`, `90°`}
import org.jro.adventofcode.y2020.Day12.{Action, Boat, Boat2, Direction, Position, puzzle1, puzzle2}
import org.jro.adventofcode.Error
import org.jro.adventofcode.y2020.Day12.Direction.{East, South}
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day12Spec extends AnyFreeSpec with Matchers with EitherValues {
	val smallSample = """F10
						|N3
						|F7
						|R90
						|F11""".stripMargin

	"Direction when" - {
		"turning left with an angle should" - {
			"change to the right direction" in {
				Direction.North.turnL(`90°`) shouldBe Direction.West
				Direction.East.turnL(`90°`) shouldBe Direction.North
				Direction.South.turnL(`90°`) shouldBe Direction.East
				Direction.West.turnL(`90°`) shouldBe Direction.South

				Direction.North.turnL(`180°`) shouldBe Direction.South
				Direction.East.turnL(`180°`) shouldBe Direction.West
				Direction.South.turnL(`180°`) shouldBe Direction.North
				Direction.West.turnL(`180°`) shouldBe Direction.East

				Direction.North.turnL(`270°`) shouldBe Direction.East
				Direction.East.turnL(`270°`) shouldBe Direction.South
				Direction.South.turnL(`270°`) shouldBe Direction.West
				Direction.West.turnL(`270°`) shouldBe Direction.North
			}
		}
		"turning right with an angle should" - {
			"change to the right direction" in {
				Direction.North.turnR(`90°`) shouldBe Direction.East
				Direction.East.turnR(`90°`) shouldBe Direction.South
				Direction.South.turnR(`90°`) shouldBe Direction.West
				Direction.West.turnR(`90°`) shouldBe Direction.North

				Direction.North.turnR(`180°`) shouldBe Direction.South
				Direction.East.turnR(`180°`) shouldBe Direction.West
				Direction.South.turnR(`180°`) shouldBe Direction.North
				Direction.West.turnR(`180°`) shouldBe Direction.East

				Direction.North.turnR(`270°`) shouldBe Direction.West
				Direction.East.turnR(`270°`) shouldBe Direction.North
				Direction.South.turnR(`270°`) shouldBe Direction.East
				Direction.West.turnR(`270°`) shouldBe Direction.South
			}
		}
	}
	"Boat when" - {
		"execution small sample actions should" - {
			"should finish at (17 ; -8)" in {
				val actualActions: Seq[Action] = Error.sequence(smallSample.split("""\n""").toIndexedSeq.map(Action.of)).value

				val finalBoat: Boat = puzzle1(actualActions)

				finalBoat shouldBe Boat(Position(17, -8), South)
				finalBoat.mahattanDistFromOrigin shouldBe 25
			}
		}
	}
	"Boat2 when" - {
		"execution small sample actions should" - {
			"should finish at (214 ; -72)" in {
				val actualActions: Seq[Action] = Error.sequence(smallSample.split("""\n""").toIndexedSeq.map(Action.of)).value

				val finalBoat: Boat2 = puzzle2(actualActions)

				finalBoat shouldBe Boat2(Position(214, -72), Position(4, -10))
				finalBoat.mahattanDistFromOrigin shouldBe 286
			}
		}
	}

	"Angle rotation should work " in {
		val waypoint = Position(10, 4)
		`90°`.rotateLeftAroundOrigin(waypoint) shouldBe Position(-4, 10)
		`180°`.rotateLeftAroundOrigin(waypoint) shouldBe Position(-10, -4)
		`270°`.rotateLeftAroundOrigin(waypoint) shouldBe Position(4, -10)

		`90°`.rotateRightAroundOrigin(waypoint) shouldBe Position(4, -10)
		`180°`.rotateRightAroundOrigin(waypoint) shouldBe Position(-10, -4)
		`270°`.rotateRightAroundOrigin(waypoint) shouldBe Position(-4, 10)
	}
}
