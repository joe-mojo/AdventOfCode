package org.jro.adventofcode.y2021

import org.jro.adventofcode
import org.jro.adventofcode.Error

import scala.collection.mutable
import scala.collection.mutable.Buffer
import Math.{max, min}
import scala.annotation.tailrec

object Day11 {

	case class Location(row: Int, col:Int)
	type Matrix[T] = mutable.IndexedSeq[mutable.IndexedSeq[T]]


	val errOrNrjLevels: Either[Error, Matrix[Int]] = adventofcode.getInputLines(2021, 11).map { lines =>
		mutable.IndexedSeq.empty[mutable.IndexedSeq[Int]].concat(lines.map { line =>
			mutable.IndexedSeq.empty[Int].concat(line.map(c => s"$c".toInt))
		})
	}

	def applyAll(octopuses: Matrix[Int], op: Int => Int): Matrix[Int] = {
		octopuses.map (row => row.map(op))
	}

	def applyAllIndexed(octopuses: Matrix[Int], op: (Matrix[Int], Location, Int) => Int): Matrix[Int] = {
		octopuses.zipWithIndex.map {
			case (row, rowIndex) => row.zipWithIndex.map {
				case (level, colIndex) => op(octopuses, Location(rowIndex, colIndex), level)
			}
		}
	}

	def getAdjacentLocs(location: Location, maxIdx: Int): Seq[Location] = {
		for {
			row <- max(location.row - 1, 0) to min(location.row + 1, maxIdx)
			col <- max(location.col - 1, 0) to min(location.col + 1, maxIdx)
		} yield Location(row, col)
	}

	def charge(octopuses: Matrix[Int]): Matrix[Int] = applyAll(octopuses, _ + 1)

	def resetFlashed(octopuses: Matrix[Int]): (Matrix[Int], Int) = {
		var flashCount = 0
		val newOctopuses = applyAll(octopuses, level => if(level > 9){ flashCount += 1; 0 } else level)
		(newOctopuses, flashCount)
	}

	def propagateFlash(octopuses: Matrix[Int]): Matrix[Int] = {
		// 1) collect flashing
		val flashing: Seq[Location] = octopuses.zipWithIndex.flatMap {
			case (row, rowIndex) => row.zipWithIndex.collect {
				case (level, colIndex) if level > 9 => Location(rowIndex, colIndex)
			}
		}.toSeq
		// 2) iterate
		@tailrec
		def doPropagate(currentFlashing: Seq[Location]): Unit = {
			val newFlashing = Buffer.empty[Location]
			currentFlashing.foreach { location =>
				getAdjacentLocs(location, octopuses.size - 1).foreach{ adjLoc =>
					octopuses(adjLoc.row)(adjLoc.col) += 1
					if(octopuses(adjLoc.row)(adjLoc.col) == 10) newFlashing.addOne(adjLoc)
				}
			}
			if(newFlashing.nonEmpty) doPropagate(newFlashing.toSeq)
		}
		doPropagate(flashing)
		octopuses
	}

	def step(octopuses: Matrix[Int]): (Matrix[Int], Int) = {
		resetFlashed(propagateFlash(charge(octopuses)))
	}

	def renderLevel(lvl: Int): String = {
		lvl match {
			case 0 => s"\u001B[97m0${Console.RESET}"
			case 9 => s"${Console.YELLOW}9${Console.RESET}"
			case n => s"\u001B[37m$n${Console.RESET}"
		}
	}

	def render(octopuses: Matrix[Int]): String = {
		octopuses.map { row =>
			row.map(renderLevel).mkString("\t", "", "\n")
		}.mkString("")
	}
	def render(res: (Matrix[Int], Int)): String = {
		render(res._1) + s"\t${res._2} flashes"
	}

	def puzzleSmallSample: Unit = {
		val smallSample: Array[mutable.IndexedSeq[Int]] =
			"""11111
			  |19991
			  |19191
			  |19991
			  |11111
			  |""".stripMargin.split("\\n").map(line => mutable.IndexedSeq.empty.concat(line.map(c => s"$c".toInt)))
		println(s"Sample:\n${render(smallSample)}")
		println(s"After step 1:\n${render(step(smallSample))}")
		println(s"After step 2:\n${render(step(step(smallSample)._1))}")
	}

	def puzzleSample: Unit = {
		var sample: mutable.IndexedSeq[mutable.IndexedSeq[Int]] =
			"""5483143223
			  |2745854711
			  |5264556173
			  |6141336146
			  |6357385478
			  |4167524645
			  |2176841721
			  |6882881134
			  |4846848554
			  |5283751526""".stripMargin.split("\\n").map(line => mutable.IndexedSeq.empty.concat(line.map(c => s"$c".toInt)))
		println(s"Sample:\n${render(sample)}")
		var totalFlashes = 0
		(1 to 10).foreach { stepNum =>
			val res = step(sample)
			sample = res._1
			totalFlashes += res._2
			println(s"After step $stepNum:\n${render(sample)}\t${res._2} flashes")
		}
		println(s"$totalFlashes total flashes")
	}

	def runNSteps(n: Int, octopuses: Matrix[Int]): (Matrix[Int], Int) = {
		println(s"Initial step:\n${render(octopuses)}")
		val finalRes = (1 to n).foldLeft((octopuses, 0)){ (octopusesAndFlashes, stepNum) =>
			val res = step(octopusesAndFlashes._1)
			println(s"After step $stepNum:\n${render(res._1)}\t${res._2} flashes")
			(res._1, octopusesAndFlashes._2 + res._2)
		}
		finalRes
	}

	def findFirstSyncFlashStep(octopuses: Matrix[Int]): Int = {
		println(s"Initial step:\n${render(octopuses)}")
		var stepNum = 1
		var res = step(octopuses)
		while(res._2 < 100) {
			stepNum += 1
			res = step(res._1)
		}
		stepNum
	}


	def main(args: Array[String]): Unit = {
		//puzzleSmallSample
		//puzzleSample
		println(s"Day11.1: ${errOrNrjLevels.map(runNSteps(100, _)._2)} flashes")
		println(s"Day11.2: first synchronized flashes after step ${errOrNrjLevels.map(findFirstSyncFlashStep)}")
	}

}
