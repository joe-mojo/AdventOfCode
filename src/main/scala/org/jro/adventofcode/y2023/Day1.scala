package org.jro.adventofcode.y2023

import org.jro.adventofcode
import org.jro.adventofcode.getInputLines

/**
 * @author joe_mojo.
 *         2023/12/01
 */
object Day1 {
	/*
	SAMPLE:
	-----------
	1abc2
	pqr3stu8vwx
	a1b2c3d4e5f
	treb7uchet
	-----------
	RESULT: 12 + 35 + 15 + 77 = 142
	 */

	def puzzle1(lines: Iterator[String]): Long = {
		lines.foldLeft(0L){ (total, line) =>
			val lineDigits = line.filter(c => c.isDigit)
			total + s"${lineDigits.head}${lineDigits.last}".toLong
		}
	}

	def replaceNumberWords(line: String): String = {//one, two, three, four, five, six, seven, eight
		line.replaceAll("one", "1").replaceAll("two", "2").
			replaceAll("three", "3").replaceAll("four", "4").
			replaceAll("five", "5").replaceAll("six", "6").
			replaceAll("seven", "7").replaceAll("eight", "8").
			replaceAll("nine", "9")
	}

	def puzzle2(lines: Iterator[String]): Long = {
		lines.map(replaceNumberWords).foldLeft(0L) { (total, line) =>
			val lineDigits = line.filter(c => c.isDigit)
			total + s"${lineDigits.head}${lineDigits.last}".toLong
		}
	}

	def main(args: Array[String]): Unit = {
		val sample: String =
			"""1abc2
				|	pqr3stu8vwx
				|	a1b2c3d4e5f
				|	treb7uchet
				|""".stripMargin
		println(s"Puzzle1 sample = ${puzzle1(sample.split("\\n").iterator)}") // 142

		getInputLines(2023, 1) match
			case Right(lines) =>
				val inputData = lines.toSeq
				println(s"Puzzle 1 = ${puzzle1(inputData.iterator)}")
				println(s"Puzzle 2 = ${puzzle2(inputData.iterator)}")
			case Left(err) =>
				println(s"Puzzle input didn't load ! Reason:\n $err")
	}

}
