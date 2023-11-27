package org.jro.adventofcode.y2020

import org.jro.adventofcode
import org.jro.adventofcode.{Error, getInputSourceOf, parseInt}
import org.jro.adventofcode.Error.UnknownEnumValue
import org.jro.adventofcode.y2020.Day12.Action.{Move, MoveE, MoveF, MoveN, Oriented, TurnL, TurnR}
import org.jro.adventofcode.y2020.Day12.Direction.{East, North, South, West, values}

import scala.annotation.targetName
import scala.util.matching.Regex

object Day12 {
	sealed trait Angle {
		def degrees: Int
		def dirDelta: Int
		def rotateLeftAroundOrigin(pos: Position): Position
		def rotateRightAroundOrigin(pos: Position): Position
		/*
		The rule of a rotation rO of 90° centered on the origin point O of the Cartesian plane, in the positive direction (counter-clockwise), is rO:(x,y)↦(−y,x).
		The rule of a rotation rO of 180° centered on the origin point O of the Cartesian plane, in the positive direction (counter-clockwise) is rO:(x,y)↦(−x,−y).
		The rule of a rotation rO of 270° centered on the origin point O of the Cartesian plane in the positive direction (counter-clockwise), is rO:(x,y)↦(y,−x).
		 */
	}
	object Angle {
		@targetName("_90deg")
		case object `90°` extends Angle {
			override val degrees = 90
			override def dirDelta: Int = 1
			override def rotateLeftAroundOrigin(pos: Position): Position = Position(-pos.y, pos.x)
			override def rotateRightAroundOrigin(pos: Position): Position = - rotateLeftAroundOrigin(pos)
		}
		@targetName("_180deg")
		case object `180°` extends Angle {
			override val degrees = 180
			override def dirDelta: Int = 2
			override def rotateLeftAroundOrigin(pos: Position): Position = Position(-pos.x, -pos.y)
			override def rotateRightAroundOrigin(pos: Position): Position = rotateLeftAroundOrigin(pos)
		}
		@targetName("_270deg")
		case object `270°` extends Angle {
			override val degrees = 270
			override def dirDelta: Int = 3
			override def rotateLeftAroundOrigin(pos: Position): Position = Position(pos.y, -pos.x)
			override def rotateRightAroundOrigin(pos: Position): Position = - rotateLeftAroundOrigin(pos)
		}

		def of(degreeValue: Int): Option[Angle] = {
			degreeValue match {
				case `90°`.degrees => Some(`90°`)
				case `180°`.degrees => Some(`180°`)
				case `270°`.degrees => Some(`270°`)
				case _ => None
			}
		}
	}
	sealed trait OrientedMove extends Action with Move with Oriented
	object OrientedMove {
		def unapply(arg: OrientedMove): Option[(Char, Int, Direction)] = {
			Some((arg.symbol, arg.units, arg.direction))
		}
	}

	sealed abstract class Action(val symbol: Char)
	object Action {
		sealed trait Move extends Action {
			def units: Int
			//def transform(pos: Position): Position
			override def toString: String = s"$symbol$units"
		}
		sealed trait Rotation extends Action {
			def angle: Angle
			override def toString: String = s"$symbol${angle.degrees}"
		}
		sealed trait Oriented extends Action {
			def direction: Direction
		}
		case class MoveN(override val units: Int) extends Action('N') with OrientedMove {
			override def direction: Direction = North
		}
		case class MoveE(override val units: Int) extends Action('E') with OrientedMove {
			override def direction: Direction = East
		}
		case class MoveS(override val units: Int) extends Action('S') with OrientedMove {
			override def direction: Direction = South
		}
		case class MoveW(override val units: Int) extends Action('W') with OrientedMove {
			override def direction: Direction = West
		}
		case class MoveF(override val units: Int) extends Action('F') with Move
		case class TurnR(override val angle: Angle) extends Action('R') with Rotation
		case class TurnL(override val angle: Angle) extends Action('L') with Rotation

		val Pattern: Regex = """([NESWFRL])(\d+)""".r

		def of(value: String): scala.util.Either[Error, Action] = {
			value match {
				case Action.Pattern("N", num) => parseInt(num).map(MoveN.apply)
				case Action.Pattern("E", num) => parseInt(num).map(MoveE.apply)
				case Action.Pattern("S", num) => parseInt(num).map(MoveS.apply)
				case Action.Pattern("W", num) => parseInt(num).map(MoveW.apply)
				case Action.Pattern("F", num) => parseInt(num).map(MoveF.apply)
				case Action.Pattern("R", num) =>
					parseInt(num).flatMap{ n =>
						Angle.of(n).fold[Either[Error, Angle]](Left(UnknownEnumValue[Angle](num)))(Right(_))
					}.map(TurnR.apply)
				case Action.Pattern("L", num) =>
					parseInt(num).flatMap{ n =>
						Angle.of(n).fold[Either[Error, Angle]](Left(UnknownEnumValue[Angle](num)))(Right(_))
					}.map(TurnL.apply)
			}
		}

	}

	case class Position(x: Long, y: Long) {
		def unary_- = Position(-x, -y)
	}

	sealed trait Direction {
		def dx: Int
		def dy: Int
		def translate(pos: Position, units: Int = 1): Position = {
			Position(
				x = pos.x + dx * units,
				y = pos.y + dy * units
			)
		}
		def ordinal: Int
		def turnL(angle: Angle): Direction = {
			values((ordinal + values.length - angle.dirDelta) % values.length)
		}
		def turnR(angle: Angle): Direction = {
			values((ordinal + angle.dirDelta) % values.length)
		}
	}
	object Direction {
		case object North extends Direction {
			override val dx: Int = 0
			override val dy: Int = 1
			override def ordinal: Int = 0
		}
		case object East extends Direction {
			override val dx: Int = 1
			override val dy: Int = 0
			override def ordinal: Int = 1
		}
		case object South extends Direction {
			override val dx: Int = 0
			override val dy: Int = -1
			override def ordinal: Int = 2
		}
		case object West extends Direction {
			override val dx: Int = -1
			override val dy: Int = 0
			override def ordinal: Int = 3
		}
		val values: IndexedSeq[Direction] = IndexedSeq(North, East, South, West)
	}

	case class Boat(pos: Position, toward: Direction){
		def mahattanDistFromOrigin: Long = Math.abs(pos.x) + Math.abs(pos.y)
		def run(action: Action): Boat = {
			action match {
				case OrientedMove(_, u, dir) => Boat(dir.translate(pos, u), toward)
				case MoveF(u) => Boat(toward.translate(pos, u), toward)
				case TurnL(angle) => Boat(pos, toward.turnL(angle))
				case TurnR(angle) => Boat(pos, toward.turnR(angle))
			}
		}
	}
	case class Boat2(pos: Position, wayPoint: Position) {
		def mahattanDistFromOrigin: Long = Math.abs(pos.x) + Math.abs(pos.y)
		def run(action: Action): Boat2 = {
			action match {
				case OrientedMove(_, u, dir) =>
					Boat2(pos, dir.translate(wayPoint, u))
				case MoveF(u) =>
					Boat2(
						Position(
							x = pos.x + wayPoint.x * u,
							y = pos.y + wayPoint.y * u
						),
						wayPoint
					)
				case TurnL(angle) =>
					Boat2(pos, angle.rotateLeftAroundOrigin(wayPoint))
				case TurnR(angle) =>
					Boat2(pos, angle.rotateRightAroundOrigin(wayPoint))
			}
		}
	}

	def puzzle1(actions: Seq[Action]): Boat = {
		actions.foldLeft(Boat(Position(0, 0), East)){ (formerBoat, action) =>
			formerBoat.run(action)
		}
	}

	def puzzle2(actions: Seq[Action]): Boat2 = {
		actions.foldLeft(Boat2(Position(0, 0), Position(10, 1))){ (formerBoat, action) =>
			formerBoat.run(action)
		}
	}

	def main(args: Array[String]): Unit = {
		val eitherActions: Either[Error, Seq[Action]] = getInputSourceOf(2020, 12).map(_.getLines()).flatMap { lines =>
			Error.sequence(lines.map(Action.of))
		}

		println(s"Actions: $eitherActions")

		val eitherFinalBoat = eitherActions.map(puzzle1)
		println(s"Final boat = $eitherFinalBoat")
		val res1 = eitherFinalBoat.map(_.mahattanDistFromOrigin)
		println(s"res1 = $res1")

		val eitherFinalBoat2 = eitherActions.map(puzzle2)
		println(s"Final boat2 = $eitherFinalBoat2")
		val res2 = eitherFinalBoat2.map(_.mahattanDistFromOrigin)
		println(s"res2 = $res2")
	}
}
