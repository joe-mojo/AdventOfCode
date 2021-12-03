package org.jro.adventofcode.y2020

import org.jro.adventofcode
import org.jro.adventofcode.Error

import scala.annotation.tailrec

object Day10 {

	def countHop(joltages: Seq[Int]): Map[Int, Int] = {
		@tailrec
		def doCount(currentValue: Int, remainingJoltages: List[Int], stats: Map[Int, Int]): Map[Int, Int] = {
			remainingJoltages match {
				case head :: Nil =>
					//This is last iteration
					val diff = head - currentValue
					stats.updatedWith(diff) {
						case None => Some(1)
						case Some(oldValue) => Some(oldValue + 1)
					}
				case head :: tail =>
					val diff = head - currentValue
					if(diff >= 1 && diff <= 3){ //diff currentValue-first Joltage OK, jump.
						doCount(
							head,
							tail,
							stats.updatedWith(diff) {
								case None => Some(1)
								case Some(oldValue) => Some(oldValue + 1)
							}
						)
					} else { //wrong diff, keep current units and find next. Not expected in input
						doCount(currentValue, tail, stats)
					}
				case Nil =>
					stats //not expected
			}
		}
		doCount(0, joltages.appended(joltages.max + 3).toList, Map.empty)
	}

	def countAllArrangementsHop(joltages: Seq[Int]): Int = {
		def doCount(currentPath: List[Int], remainingJoltages: List[Int], resCache: Map[Int, Int]): Int = {
			???
		}
		???
	}

	def main(args: Array[String]): Unit = {

		val maybeJoltages: Either[Error, Seq[Int]] = adventofcode.getInputSourceOf(2020, 10)
			.map(_.getLines().map(_.toInt).toSeq.sorted)

		val res1: Either[Error, Option[Int]] = maybeJoltages.map(countHop).map { stats =>
			println(s"Part 1 stats = $stats")
			for {
				diff1 <- stats.get(1)
				diff3 <- stats.get(3)
			} yield diff1 * diff3
		}

		println(s"res1 = $res1")

	}
}
