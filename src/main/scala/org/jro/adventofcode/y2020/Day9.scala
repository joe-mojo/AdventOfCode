package org.jro.adventofcode.y2020

import org.jro.adventofcode
import org.jro.adventofcode.Error

object Day9 {

	def findSumOf2(sum: Long, candidates: Seq[Long]): Seq[(Long, Long)] = {
		candidates.combinations(2).filter(combo => combo.sum == sum).map(rightCombo => rightCombo.head -> rightCombo.tail.head).toSeq
	}

	def findSumOfN(sum: Long, candidates: Seq[Long], sumLength: Int): Option[Seq[Long]] = {
		candidates.combinations(sumLength).find(combo => combo.sum == sum)
	}

	def isValidFor25(target: Long, previousNumbers: Seq[Long]): Boolean = isValid(target, previousNumbers, 25)

	def isValid(target: Long, previousNumbers: Seq[Long], maxPrevious: Int): Boolean = {
		findSumOf2(target, previousNumbers.takeRight(maxPrevious)).nonEmpty
	}

	def findSumOfConsecutiveN(sum: Long, candidates: Seq[Long], sumLength: Int): Seq[Seq[Long]] = {
		candidates.sliding(sumLength).filter(combo => combo.sum == sum).toSeq
	}

	def findSumOfSuites(sum: Long, candidates: Seq[Long]): Seq[Seq[Long]] = {
		(2 to candidates.length).flatMap { currenLength =>
			findSumOfConsecutiveN(sum, candidates, currenLength)
		}
	}

	def main(args: Array[String]): Unit = {

		val maybeInputNmbers: Either[Error, Seq[Long]] = adventofcode.getInputSourceOf(2020, 9)
			.map(_.getLines())
			.map(_.map(_.toLong))
			.map(_.toSeq)

		val res1 = maybeInputNmbers.map { numbers =>
			numbers.sliding(26).find { `26numbers` =>
				!isValidFor25(`26numbers`.last, `26numbers`.take(25))
			}.map(_.last)
		}

		println(s"res1 = $res1")

		val res2 = maybeInputNmbers.map { numberIter =>
			val numberSeq = numberIter.toSeq
			numberSeq.zipWithIndex.sliding(26).find { `26numbers` =>
				!isValidFor25(`26numbers`.last._1, `26numbers`.take(25).map(_._1))
			}.map(_.last).map { lastAndIndexOfLast =>
				val (targetSum, indexOfLast) = lastAndIndexOfLast
				val candidates = numberSeq.slice(0, indexOfLast).toSeq
				findSumOfSuites(numberSeq.toSeq(indexOfLast), candidates).map(combo => combo.min + combo.max)
			}
		}

		println(s"res2 = $res2")

	}

}
