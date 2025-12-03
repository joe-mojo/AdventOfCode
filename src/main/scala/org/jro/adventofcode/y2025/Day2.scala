package org.jro.adventofcode.y2025

import org.jro.adventofcode
import org.jro.adventofcode.Error.{Errors, sequence}
import org.jro.adventofcode.{Error, InputData, parseInt}

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
/**
 * @author joe_mojo.
 *         2025/12/01
 */
object Day2 {

	private[y2025] object Part1 {
		def isNotValidId(id: Long): Boolean = {
			!isValidId(id.toString)
		}
		def isValidId(id: Long): Boolean = {
			isValidId(id.toString)
		}

		def isValidId(id: String): Boolean = {
			id match {
				case anyString if anyString.length % 2 != 0 => true
				case evenString =>
					val mid = evenString.length / 2
					evenString.substring(0, mid) != evenString.substring(mid)
			}
		}
	}

	private[y2025] object Part2 {
		def isNotValidId(id: Long): Boolean = {
			!isValidId(id.toString)
		}

		def isValidId(id: Long): Boolean = {
			isValidId(id.toString)
		}

		def isValidId(id: String): Boolean = {
			isValidIdForPatternSize(id, id.length / 2)
		}

		@tailrec
		private def isValidIdForPatternSize(id: String, patternSize: Int): Boolean = {
			if(patternSize > (id.length/2) || patternSize < 1) true
			else if (id.length % patternSize != 0 || hasAtLeastOneNonRepeatedPattern(id, patternSize)) {
				isValidIdForPatternSize(id, patternSize - 1)
			} else false
		}

		private def hasAtLeastOneNonRepeatedPattern(id: String, patternSize: Int): Boolean = {
			id.grouped(patternSize).sliding(2).count(pair => pair.head != pair.last) > 0
		}
	}


	private[y2025] def parseInput(lines: Iterator[String]): Either[Error, Seq[NumericRange[Long]]] = {
		sequence(lines.flatMap(_.split(',')).map(rangeStr => {
			val dash = "-".r
			val bounds = dash.split(rangeStr).map(_.trim.toLong)
			if (bounds.length != 2) Left(Error.WrongFixedSplit(rangeStr, dash, 2, bounds.length))
			else Right(bounds.head to bounds.last)
		}))
	}

	private def parseInput(input: InputData): Either[Error, Seq[NumericRange[Long]]] = {
		parseInput(input.source.getLines())
	}

	def puzzle1(input: Seq[NumericRange[Long]]): Long = {
		input.flatMap(range => range.filter(Part1.isNotValidId)).sum
	}

	def puzzle2(input: Seq[NumericRange[Long]]): Long = {
		input.flatMap(range => range.filter(Part2.isNotValidId)).sum
	}

	def main(args: Array[String]): Unit = adventofcode.y2025.mainWithTransformer(2, puzzle1 /*OK: 19128774598*/, puzzle2 /*OK: 21932258645*/, parseInput)
}
