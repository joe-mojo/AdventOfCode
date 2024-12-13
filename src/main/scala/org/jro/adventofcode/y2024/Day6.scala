package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.IOError
import org.jro.adventofcode.y2024.Day6.Direction.East

import scala.annotation.{tailrec, targetName}
import scala.util.Using

/**
 * @author joe_mojo.
 *         2024/12/13
 */
object Day6 {


	enum Direction(val dx: Int, val dy: Int) {
		case North extends Direction(0, -1)
		case East extends Direction(1, 0)
		case South extends Direction(0, 1)
		case West extends Direction(-1, 0)

		def turnRight: Direction = this match {
			case North => East
			case East => South
			case South => West
			case West => North
		}

		def opposite: Direction = this match {
			case North => South
			case East => West
			case South => North
			case West => East
		}
	}
	object Direction {
		def fromChar(c: Char): Option[Direction] = c match {
			case '^' => Some(Direction.North)
			case '>' => Some(Direction.East)
			case 'v' => Some(Direction.South)
			case 'V' => Some(Direction.South)
			case '<' => Some(Direction.West)
			case _ => None
		}
	}

	case class Location(x: Int, y: Int) {
		@targetName("plus")
		def +(dx: Int, dy: Int): Location = Location(x + dx, y + dy)
		def stepTo(direction: Direction): Location = this + (direction.dx, direction.dy)
	}

	sealed trait Guard {
		def location: Location
		def direction: Direction
	}
	case class MovingGuard(override val location: Location, direction: Direction) extends Guard {
		def goInFrontOfObstacleAt(obstacleLocation: Location): MovingGuard = copy(
			location = obstacleLocation.stepTo(direction.opposite),
			direction = direction.turnRight
		)

		def isInFootPrint(footPrint: FootPrint): Boolean = {
			location == footPrint.location  && direction == footPrint.direction
		}
	}
	case class ExitingGuard(override val location: Location, override val direction: Direction) extends Guard {
	}

	case class FootPrint(location: Location, direction: Direction) {
		def inFrontOfObstacle(obstacleLocation: Location): FootPrint = copy(
			location = obstacleLocation.stepTo(direction.opposite)
		)
		def turnRight: FootPrint = copy(direction = direction.turnRight)
	}
	object FootPrint {
		def of(guard: Guard): FootPrint = FootPrint(guard.location, guard.direction)
	}

	case class Room(obstacles: Set[Location], sideLength: Int) {
		val obstaclesByLine: Map[Int, Set[Location]] = obstacles.groupBy(_.y)
		val obstaclesByColumn: Map[Int, Set[Location]] = obstacles.groupBy(_.x)

		def nextObstacle(location: Location, direction: Direction): Option[Location] = {
			direction match
				case Direction.North =>
					obstaclesByColumn.get(location.x).flatMap(_.filter(_.y < location.y).maxByOption(_.y))
				case Direction.South =>
					obstaclesByColumn.get(location.x).flatMap(_.filter(_.y > location.y).minByOption(_.y))
				case Direction.East =>
					obstaclesByLine.get(location.y).flatMap(_.filter(_.x > location.x).minByOption(_.x))
				case Direction.West =>
					obstaclesByLine.get(location.y).flatMap(_.filter(_.x < location.x).maxByOption(_.x))
		}

		def exit(movingGuard: MovingGuard): ExitingGuard = {
			movingGuard.direction match
				case Direction.North => ExitingGuard(Location(movingGuard.location.x, 0), movingGuard.direction)
				case Direction.East => ExitingGuard(Location(sideLength - 1, movingGuard.location.y), movingGuard.direction)
				case Direction.South => ExitingGuard(Location(movingGuard.location.x, sideLength - 1), movingGuard.direction)
				case Direction.West => ExitingGuard(Location(0, movingGuard.location.y), movingGuard.direction)
		}

	}

	private[y2024] def getObstacles(input: IndexedSeq[String]): Set[Location] = {
		input.zipWithIndex.flatMap { (line, y) =>
			line.zipWithIndex.collect {
				case ('#', x) => Location(x, y)
			}
		}.toSet
	}

	private[y2024] def findStartLocation(inputSeq: IndexedSeq[String], room: Room): Guard = {
		(for {
			x <- 0 until room.sideLength
			y <- 0 until room.sideLength
			currentTile = inputSeq(y)(x)
			direction <- Direction.fromChar(currentTile)
		} yield MovingGuard(Location(x, y), direction)).head // Assumed: there is exactly one guard
	}

	def puzzle1(room: Room, start: Guard): Int = {
		val seenLocations: Set[Location] = Iterator.iterate[Option[(Room, Guard)]](Some(room, start)) {
			case Some((room, guard:MovingGuard)) =>
				val maybeNextObstacle = room.nextObstacle(guard.location, guard.direction)
				maybeNextObstacle match {
					case Some(location) => Some(room -> guard.goInFrontOfObstacleAt(location))
					case None => Some(room -> room.exit(guard)) // No obstacle ahead
				}
			case Some((room, guard:ExitingGuard)) => None
			case None => None
		}.takeWhile {
			_.isDefined
		}.flatMap {
			case Some(_, guard) => Some(guard.location)
			case None => None
		}.sliding(2).flatMap{startStop =>
			val start = startStop.head
			val stop = startStop.last
			for {
				x <- Math.min(start.x, stop.x) to Math.max(start.x, stop.x)
				y <- Math.min(start.y, stop.y) to Math.max(start.y, stop.y)
			} yield Location(x, y)
		}.toSet

		seenLocations.size
	}

	def generateFootPrints(room: Room, start: Guard): Seq[FootPrint] = {
		@tailrec
		def genateFootPrintsRec(room:Room, start: Guard, track: Seq[FootPrint], seen: Set[FootPrint]): Seq[FootPrint] = {
			val newFootPrint = FootPrint.of(start)
			if(seen.contains(newFootPrint)) Seq.empty // Loop detected
			else start match {
				case guard@MovingGuard(loc, dir) =>
					val maybeNextObstacle = room.nextObstacle(loc, dir)
					maybeNextObstacle match {
						case Some(location) =>
							val nextGuard = guard.goInFrontOfObstacleAt(location)
							val nextFootPrint = FootPrint.of(nextGuard)
							genateFootPrintsRec(room, nextGuard, track :+ newFootPrint, seen + newFootPrint)
						case None =>
							val nextGuard = room.exit(guard)
							val nextFootPrint = FootPrint.of(nextGuard)
							genateFootPrintsRec(room, nextGuard, track :+ newFootPrint, seen + newFootPrint)
					}
				case ExitingGuard(loc, dir) =>
					track :+ FootPrint(loc, dir)
			}
		}

		val footPrints: Seq[FootPrint] = genateFootPrintsRec(room, start, Seq.empty, Set.empty)

		footPrints.sliding(2).flatMap { startStop =>
			val start = startStop.head
			val stop = startStop.last

			for {
				x <- (start.location.x to stop.location.x) by (if (start.direction.dx == 0) 1 else start.direction.dx)
				y <- (start.location.y to stop.location.y) by (if (start.direction.dy == 0) 1 else start.direction.dy)
			} yield FootPrint(Location(x, y), start.direction)
		}.toSeq

	}

	def puzzle2(room: Room, start: Guard): Int = {
		val footPrints: Seq[FootPrint] = generateFootPrints(room, start)

		//val footPrintDirections: Map[Location, Set[Direction]] = footPrints.groupBy(_.location).view.mapValues(_.map(_.direction).toSet).toMap

		val possibleDivertingLocations = footPrints.sliding(2).flatMap {
			case Seq(footPrint1, footPrint2) if footPrint1.location == footPrint2.location || footPrint2.location == start.location => None
			case Seq(footPrint1, footPrint2) =>
				val divertingObstacleLocation = footPrint2.location
				val newGardRound = generateFootPrints(room.copy(obstacles = room.obstacles + divertingObstacleLocation), start)
				if(newGardRound.isEmpty) Some(divertingObstacleLocation)
				else None
			case _ => None
		}.toSet
		possibleDivertingLocations.size
	}


	def main(args: Array[String]): Unit = {
		adventofcode.getInputData(2024, 6).flatMap { inputData =>
			Using(inputData.source) { source =>
				source.getLines().toIndexedSeq
			}.toEither.left.map(throwable => IOError(inputData.resource, throwable))
		} match {
			case Right(inputLines) =>
				val room = Room(getObstacles(inputLines), inputLines.size)
				val start = findStartLocation(inputLines, room)
				println(s"Puzzle 1 = ${puzzle1(room, start)}") // 4939 OK
				println(s"Puzzle 2 = ${puzzle2(room, start)}") // 1434 OK
			case Left(err) =>
				println(s"Puzzle input didn't load ! Reason:\n $err")
		}
	}
}
