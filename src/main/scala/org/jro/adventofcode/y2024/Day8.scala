package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.IOError

import scala.util.Using

object Day8 {

	case class Antenna(location: Location, frequencyId: Char) {
		def antinodesWith(other: Antenna): Set[Antinode] = Set(
				Antinode(Location(location.x - (other.location.x - location.x), location.y - (other.location.y - location.y))),
				Antinode(Location(other.location.x + (other.location.x - location.x), other.location.y + (other.location.y - location.y)))
			)
	}

	case class Antinode(location: Location)

	case class Location(x: Int, y: Int)

	case class Field(sideLength: Int, antennas: Map[Location, Antenna], antinodes: Set[Antinode] = Set.empty) {
		def antinodesAround(antenna1: Antenna, antenna2: Antenna): Set[Antinode] =
			antenna1.antinodesWith(antenna2).filter(antinode =>
				antinode.location.x >= 0 && antinode.location.y >= 0 && antinode.location.x < sideLength && antinode.location.y < sideLength
			)

		def antennaPairs: Set[(Antenna, Antenna)] = antennas.groupBy(_._2.frequencyId).values.flatMap { oneFreqAntennas =>
			oneFreqAntennas.values.toSeq.combinations(2).map { case Seq(antenna1, antenna2) => (antenna1, antenna2) }
		}.toSet
	}

	def parseField(lines: IndexedSeq[String]): Field = {
		val sideLength = lines.length
		val antennas = lines.zipWithIndex.flatMap { (line, y) =>
			line.zipWithIndex.collect {
				case (char, x) if char != '.' =>
					val tileLoc = Location(x, y)
					tileLoc -> Antenna(tileLoc, char)
			}
		}.toMap
		/*val antinodes = lines.tail.drop(sideLength + 1).take(sideLength).zipWithIndex.flatMap { (line, y) =>
			line.zipWithIndex.collect { case ('X', x) => Location(x, y) }
		}.toSet*/

		Field(sideLength, antennas)
	}


	def puzzle1(field: Field): Int = {
		//TODO: Implement puzzle 1
		0
	}


	def main(args: Array[String]): Unit = {
		adventofcode.getInputData(2024, 8).flatMap { inputData =>
			Using(inputData.source) { source =>
				parseField(source.getLines().toIndexedSeq)
			}.toEither.left.map(throwable => IOError(inputData.resource, throwable))
		} match
			case Right(initialField) =>
				println(s"Puzzle 1 = ${puzzle1(initialField)}")
			//println(s"Puzzle 2 = ${puzzle2(initialField)}")
			case Left(error) =>
				println(s"Error: $error")
	}
}
