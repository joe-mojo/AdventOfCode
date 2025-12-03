package org.jro.adventofcode.y2025

import org.jro.adventofcode
import org.jro.adventofcode.Error.UnexpectedError
import org.jro.adventofcode.{Error, InputData, NsTimer}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
 * @author joe_mojo.
 *         2025/12/03
 */
object Day3 {

	case class BatteryBank(joltages: IndexedSeq[Int]) {
		def combinedJoltage(firstIndex: Int, secondIndex: Int): Int = {
			10 * joltages(firstIndex) + joltages(secondIndex)
		}

		def combined2JoltagesIterator: Iterator[Int] = new Iterator[Int] {
			private var firstIndex = 0
			private var secondIndex = 1

			override def hasNext: Boolean = {
				firstIndex < (joltages.length - 1) && secondIndex < joltages.length
			}

			override def next(): Int = {
				val combined = combinedJoltage(firstIndex, secondIndex)
				secondIndex += 1
				if (secondIndex >= joltages.length) {
					firstIndex += 1
					secondIndex = firstIndex + 1
				}
				combined
			}
		}

		def combinedJoltage(indices: Seq[Int]): Long = {
			indices.iterator.map(idx => joltages(idx).toLong).zipWithIndex.map(indexAndRank =>
				indexAndRank._1 * Math.pow(10, indices.length - indexAndRank._2 - 1).longValue
			).sum
		}

		private[y2025] def maxCombinedJoltageRecFast(selectedIndices: List[Int] = List.empty, startIndex: Int = 0, remainingSelectionSize: Int, currentMax: Long = 0L): Long = {
			if (remainingSelectionSize == 0) {
				val combined = combinedJoltage(selectedIndices)
				Math.max(currentMax, combined)
			} else {
				val maxJoltageIndices = findMaxJoltageIndices(startIndex, remainingSelectionSize)
				maxJoltageIndices.iterator.map { nextIndex =>
					maxCombinedJoltageRecFast(
						selectedIndices :+ nextIndex,
						nextIndex + 1,
						remainingSelectionSize - 1,
						currentMax
					)
				}.foldLeft(currentMax)(Math.max)
			}
		}

		private[y2025] def findMaxJoltageIndices(startIndex: Int, remainingSelectionSize: Int): Seq[Int] = {
			val indexMax = joltages.length - remainingSelectionSize
			val joltagesSliceWithIndices = joltages.zipWithIndex.slice(startIndex, indexMax + 1)
			val maxJoltage = joltagesSliceWithIndices.map(_._1).max
			joltagesSliceWithIndices.filter(_._1 == maxJoltage).map(_._2)
		}

		def maxCombined12Joltage: Long = {
			//maxCombinedJoltageRecLight(remainingIndices = joltages.indices, remainingSelectionSize = 12)
			maxCombinedJoltageRecFast(remainingSelectionSize = 12)
		}
	}

	object  BatteryBank {
		def unsafeParse(digits: String): BatteryBank = {
			BatteryBank(digits.map(Character.digit(_, 10)))
		}
	}

	private[y2025] def parseInput(input: InputData): Either[Error, Seq[BatteryBank]] =
		Try(input.source.getLines().map(BatteryBank.unsafeParse).toSeq).toEither.left.map(UnexpectedError(_))

	def puzzle1(input: Seq[BatteryBank]): Long = {
		input.map { bank =>
			bank.combined2JoltagesIterator.max
		}.map(_.toLong).sum
	}

	def puzzle2(input: Seq[BatteryBank]): Long = {
		val es = java.util.concurrent.Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
		implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(es)
		val timer = new NsTimer
		val resultSeq: Seq[Long] = scala.concurrent.Await.result(
			Future.sequence(input.map { bank =>
				Future(bank.maxCombined12Joltage)
			}), scala.concurrent.duration.Duration.Inf)

		/*
		val resultSeq: Seq[Long] = input.map { bank =>
			bank.maxCombined12Joltage
		}*/
		val res = resultSeq.sum
		val elapsed = timer.elapsedMillis()
		println(s"Day3.puzzle2 took $elapsed ms")
		es.shutdown()
		res
	}

	def main(args: Array[String]): Unit = adventofcode.y2025.mainWithTransformer(3, puzzle1 , puzzle2 , parseInput)
	//OK: 1) 17432	2) 173065202451341 in ~2015ms (parallelism enabled) or ~4900ms (no parallelism)
}
