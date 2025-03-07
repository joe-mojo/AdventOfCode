package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.{Errors, IOError, InputError, NaN, sequence}
import org.jro.adventofcode.InputData
import org.jro.adventofcode.y2024.mainWithTransformer

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Try
import scala.collection.mutable.Map as MutableMap

/**
 * @author joe_mojo.
 *         2025/01/13
 */
object Day12 {
	val IdGen: AtomicInteger = AtomicInteger(0)

	case class Coords(x: Int, y: Int)
	sealed trait Plow {
		def coords: Coords
		def label: Char
		def hasTopFence: Boolean
		def hasBottomFence: Boolean
		def hasRightFence: Boolean
		def hasLeftFence: Boolean
		def hasFences: Boolean
	}
	case class LocatedPlow(override val coords: Coords, override val label: Char, fences: Byte, neighboursCoords: Set[Coords]) extends Plow {
		override lazy val hasTopFence: Boolean = !neighboursCoords.contains(Coords(coords.x, coords.y - 1))
		override lazy val hasBottomFence: Boolean = !neighboursCoords.contains(Coords(coords.x, coords.y + 1))
		override lazy val hasRightFence: Boolean = !neighboursCoords.contains(Coords(coords.x + 1, coords.y))
		override lazy val hasLeftFence: Boolean = !neighboursCoords.contains(Coords(coords.x - 1, coords.y))
		override lazy val hasFences: Boolean = fences > 0
	}
	case class Side[S <: Side.Position](position: S, coords: Set[Coords])
	object Side {
		sealed trait Position
		case object Top extends Position
		case object Right extends Position
		case object Bottom extends Position
		case object Left extends Position
	}
	class SideAccumulator {
		private val topSides: mutable.HashSet[Side[Side.Top.type]] = mutable.HashSet.empty
		private val rightSides: mutable.HashSet[Side[Side.Right.type]] = mutable.HashSet.empty
		private val bottomSides: mutable.HashSet[Side[Side.Bottom.type]] = mutable.HashSet.empty
		private val leftSides: mutable.HashSet[Side[Side.Left.type]] = mutable.HashSet.empty

		def addSide(side: Side[?]): SideAccumulator = {
			side match {
				case side@Side[Side.Top.type](Side.Top, _) => topSides += side
				case side@Side[Side.Right.type](Side.Right, _) => rightSides += side
				case side@Side[Side.Bottom.type](Side.Bottom, _) => bottomSides += side
				case side@Side[Side.Left.type](Side.Left, _) => leftSides += side
			}
			this
		}
		def addPlow(plow: LocatedPlow): SideAccumulator = {
			// TODO For each side type (Top, Right, Bottom, Left) for which the plow has a fence, add the plow to the existing side holding one of its neighbours of create the side

			this
		}
	}
	case class GardenPlot(plows: HashSet[LocatedPlow]) {
		val id: String = GardenPlot.generateNextId(plows.head.label)
		val area: Int = plows.size
		def fences: Int = plows.toSeq.map(_.fences.toInt).sum
		def fenceCost: Long = fences * area
		def bulkFenceCost: Long = sides.toLong * area
		def sides: Int = ???
		private def findSides(): Int = {

			// TODO
			???
		}
	}
	object GardenPlot {
		private val nextIdByLabel: MutableMap[Char, Int] = mutable.HashMap.empty
		private def generateNextId(label: Char): String = {
			label.toString + String.format("%010d", nextIdByLabel.get(label) match {
				case Some(id) =>
					nextIdByLabel(label) = id + 1
					id
				case None =>
					nextIdByLabel(label) = 1
					0
			})
		}
	}
	case class Field(plows: Map[Coords, LocatedPlow]) {

		val gardenPlots: Set[GardenPlot] = findGardenPlots()

		private def findGardenPlots(): Set[GardenPlot] = {
			val visitedPlows = mutable.HashSet.empty[LocatedPlow]
			plows.values.flatMap(plow => findGardenPlotOf(plow, visitedPlows)).toSet
		}

		private def findGardenPlotOf(plow: LocatedPlow, visitedPlows: mutable.HashSet[LocatedPlow]): Option[GardenPlot] = {
			if(visitedPlows.contains(plow)) {
				None
			} else {
				def collectNeighbours(startingPlow: LocatedPlow, collectedPlows: mutable.HashSet[LocatedPlow]): mutable.HashSet[LocatedPlow] = {
					val newNeighbours = startingPlow.neighboursCoords.flatMap(plows.get).diff(visitedPlows)
					visitedPlows ++= newNeighbours
					collectedPlows ++= newNeighbours
					newNeighbours.map { plow =>
						collectNeighbours(plow, collectedPlows)
					}.foldLeft(mutable.HashSet.empty[LocatedPlow])(_ ++ _)
					collectedPlows
				}
				visitedPlows += plow
				val collectedPlows = collectNeighbours(plow, mutable.HashSet[LocatedPlow](plow))
				Some(GardenPlot(collectedPlows.to(HashSet)))
			}
		}

		def fenceCost: Long = gardenPlots.toSeq.map(_.fenceCost).sum

		def bulkFenceCost: Long = gardenPlots.toSeq.map(_.bulkFenceCost).sum
	}


	private def countFences(field: IndexedSeq[String], x: Int, y: Int): Byte = {
		(for {
			(dx, dy) <- Seq((1, 0), (0, 1), (-1, 0), (0, -1))
		} yield {
			(x + dx, y + dy) match {
				case (neighbourX, neighbourY) if neighbourX < 0 || neighbourX >= field.head.length || neighbourY < 0 || neighbourY >= field.length => 1
				case (neighbourX, neighbourY) if field(neighbourY)(neighbourX) != field(y)(x) =>  1
				case _ => 0
			}
		}).sum.toByte
	}

	private def collectNeighbourCoords(field: IndexedSeq[String], x: Int, y: Int): Set[Coords] = {
		(for {
			(dx, dy) <- Seq((1, 0), (0, 1), (-1, 0), (0, -1))
			(neighbourX, neighbourY) = (x + dx, y + dy)
			if neighbourX >= 0 && neighbourX < field.head.length && neighbourY >= 0 && neighbourY < field.length
			if field(neighbourY)(neighbourX) == field(y)(x)
		} yield Coords(neighbourX, neighbourY)).toSet
	}


	private[y2024] def parseInput(input: InputData): Either[Errors, IndexedSeq[String]] = {
		Try(input.source.getLines().toIndexedSeq).toEither.left.map(IOError(input.resource, _).asNEL)
	}

	private[y2024] def createPlowsMap(input: IndexedSeq[String]): Map[Coords, LocatedPlow] = {
		(for {
			(line, y) <- input.zipWithIndex
			(char, x) <- line.zipWithIndex
			coords = Coords(x, y)
		} yield {
			coords -> LocatedPlow(coords, char, countFences(input, x, y), collectNeighbourCoords(input, x, y))
		}).toMap
	}

	def puzzle1(input: IndexedSeq[String]): Long = {
		val locatedPlows = createPlowsMap(input)
		val field = Field(locatedPlows)
		field.fenceCost
	}

	def puzzle2(input: IndexedSeq[String]): Long = {
		val locatedPlows = createPlowsMap(input)
		val field = Field(locatedPlows)
		field.bulkFenceCost
	}

	def main(args: Array[String]): Unit = {
		mainWithTransformer(12, puzzle1, puzzle2, parseInput)
	}
}
