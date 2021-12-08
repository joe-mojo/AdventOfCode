package org.jro.adventofcode.y2021

import org.jro.adventofcode
import org.jro.adventofcode.Error

object Day8 {

	case class Day8Entry(uniqueSignals: Seq[String], outputDigits: (String, String, String, String))

	val segmentsToDigit: Map[String, Int] = Map(
		"abcefg"  -> 0,
		"cf" 	  -> 1,
		"acdeg"   -> 2,
		"acdfg"   -> 3,
		"bcdf" 	  -> 4,
		"abdfg"   -> 5,
		"abdefg"  -> 6,
		"acf" 	  -> 7,
		"abcdefg" -> 8,
		"abcdfg"  -> 9,
	)
	val digitToSegments: Map[Int, String] = segmentsToDigit.map(entry => entry._2 -> entry._1)

	val inputEntries: Either[Error, Seq[Day8Entry]] = adventofcode.getInputLines(2021, 8).map { lines =>
		lines.map { line =>
			val splitLine = line.split(" \\| ")
			val digits = splitLine(1).split(" ")
			val entry = Day8Entry(splitLine.head.split(" "), (digits.head, digits(1), digits(2), digits(3)))
			println(entry)
			entry
		}.toSeq
	}

	def puzzleOne(entries: Seq[Day8Entry]): Int = {
		entries.map(_.outputDigits.productIterator.asInstanceOf[Iterator[String]].map (_.length)).flatten.count { length =>
			//1				4				7			8
			length == 2 || length == 4 || length == 3 || length == 7
		}
	}

	def main(args: Array[String]): Unit = {
		println(s"Day8.1: ${inputEntries.map(puzzleOne)}")
	}

}
