package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.IOError

import scala.annotation.tailrec
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

		def withAntinodes: Field = copy(antinodes = antennaPairs.flatMap { case (antenna1, antenna2) => antinodesAround(antenna1, antenna2) })

		def withAntinodesEchoes: Field = copy(antinodes = antennaPairs.flatMap{ case (antenna1, antenna2) => Field.generateAntinodesWithEchoes(antenna1, antenna2, sideLength) })

	}

	object Field {
		def generateAntinodesWithEchoes(antenna1: Antenna, antenna2: Antenna, fieldSideLength: Int): Set[Antinode] = {
			@tailrec
			def generateAntinodesWithEchoesRec(location1: Location, location2: Location, antinodes: Set[Antinode]): Set[Antinode] = {
				val newAntinode = Antinode(Location(location2.x + (location2.x - location1.x), location2.y + (location2.y - location1.y)))
				if (newAntinode.location.x < 0 || newAntinode.location.y < 0 || newAntinode.location.x >= fieldSideLength || newAntinode.location.y >= fieldSideLength) {
					antinodes
				} else {
					generateAntinodesWithEchoesRec(location2, newAntinode.location, antinodes + newAntinode)
				}
			}
			generateAntinodesWithEchoesRec(antenna1.location, antenna2.location, Set.empty) ++
					generateAntinodesWithEchoesRec(antenna2.location, antenna1.location, Set.empty) +
					Antinode(antenna1.location) + Antinode(antenna2.location)
		}
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

		Field(sideLength, antennas)
	}


	def puzzle1(field: Field): Int = {
		field.withAntinodes.antinodes.size
	}

	def puzzle2(field: Field): Int = {
		field.withAntinodesEchoes.antinodes.size
	}


	def main(args: Array[String]): Unit = {
		adventofcode.getInputData(2024, 8).flatMap { inputData =>
			Using(inputData.source) { source =>
				parseField(source.getLines().toIndexedSeq)
			}.toEither.left.map(throwable => IOError(inputData.resource, throwable))
		} match
			case Right(initialField) =>
				println(s"Puzzle 1 = ${puzzle1(initialField)}") // 291 OK
				println(s"Puzzle 2 = ${puzzle2(initialField)}") // 1015 OK
			case Left(error) =>
				println(s"Error: $error")
	}
}
