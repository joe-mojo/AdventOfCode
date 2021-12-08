package org.jro.adventofcode.y2021

import org.jro.adventofcode
import Math.abs

object Day7 {

	val inputValues: Either[adventofcode.Error, Seq[Int]] = adventofcode.getInputData(2021, 7).map(_.source.getLines().flatMap(_.split(",")).map(_.toInt)).map(_.toSeq)

	def unitFuelCost1(from: Int, to: Int): Int = {
		abs(from - to)
	}

	def unitFuelCost2(from: Int, to: Int): Int = {
		val d = abs(from - to)
		(d * (d + 1)) / 2
	}

	def totalFuelCost(crabPositions: Seq[Int], targetPos: Int, unitFuelCost: (Int, Int) => Int): Int = {
		crabPositions.map(crabPos => unitFuelCost(crabPos, targetPos)).sum
	}

	def puzzle(crabPositions: Seq[Int], unitFuelCost: (Int, Int) => Int): (Int, Int) = {
		val sortedCraPos = crabPositions.sorted
		(sortedCraPos.head to sortedCraPos.last).map {targetPos =>
			targetPos -> totalFuelCost(crabPositions, targetPos, unitFuelCost)
		}.minBy(_._2)
	}

	def main(args: Array[String]): Unit = {
		println(inputValues.map(_.toSeq))
		println(s"Sample cost for pos 2: ${totalFuelCost(Seq(16,1,2,0,4,2,7,1,2,14), 2, unitFuelCost1)}")
		println(s"Sample cumulative cost for pos 2: ${totalFuelCost(Seq(16,1,2,0,4,2,7,1,2,14), 2, unitFuelCost2)}")
		println(s"Sample cumulative cost for pos 5: ${totalFuelCost(Seq(16,1,2,0,4,2,7,1,2,14), 5, unitFuelCost2)}")
		println(s"Day7.1: ${inputValues.map(puzzle(_, unitFuelCost1))}")
		println(s"Day7.1: ${inputValues.map(puzzle(_, unitFuelCost2))}")
	}
}
