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
	}
	case class LocatedPlow(override val coords: Coords, override val label: Char, fences: Byte, neighboursCoords: Set[Coords]) extends Plow
	case class GardenPlot(plows: HashSet[LocatedPlow]) {
		val id: String = GardenPlot.generateNextId(plows.head.label)
		val area: Int = plows.size
		def fences: Int = plows.toSeq.map(_.fences.toInt).sum
		def fenceCost: Long = fences * area
	}
	object GardenPlot {
		private val nextIdByLabel: MutableMap[Char, Int] = mutable.HashMap.empty
		def generateNextId(label: Char): String = {
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

		def findGardenPlots(): Set[GardenPlot] = {
			val visitedPlows = mutable.HashSet.empty[LocatedPlow]
			plows.values.flatMap(plow => findGardenPlotOf(plow, visitedPlows)).toSet
		}

		def findGardenPlotOf(plow: LocatedPlow, visitedPlows: mutable.HashSet[LocatedPlow]): Option[GardenPlot] = {
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

	}


	def countFences(field: IndexedSeq[String], x: Int, y: Int): Byte = {
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

	def collectNeighbourCoords(field: IndexedSeq[String], x: Int, y: Int): Set[Coords] = {
		(for {
			(dx, dy) <- Seq((1, 0), (0, 1), (-1, 0), (0, -1))
			(neighbourX, neighbourY) = (x + dx, y + dy)
			if neighbourX >= 0 && neighbourX < field.head.length && neighbourY >= 0 && neighbourY < field.length
			if field(neighbourY)(neighbourX) == field(y)(x)
		} yield Coords(neighbourX, neighbourY)).toSet
	}


	def parseInput(input: InputData): Either[Errors, IndexedSeq[String]] = {
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
	def main(args: Array[String]): Unit = {
		mainWithTransformer(12, puzzle1, (input: IndexedSeq[String]) => 0, parseInput)
	}
}
