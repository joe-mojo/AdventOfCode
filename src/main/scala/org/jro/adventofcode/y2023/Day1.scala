package org.jro.adventofcode.y2023

import org.jro.adventofcode
import org.jro.adventofcode.getInputLines

import scala.annotation.tailrec

/**
 * @author joe_mojo.
 *         2023/12/01
 */
object Day1 {

	def puzzle1(lines: Iterator[String]): Long = {
		lines.foldLeft(0L){ (total, line) =>
			val lineDigits = line.filter(c => c.isDigit)
			total + s"${lineDigits.head}${lineDigits.last}".toLong
		}
	}
	private object StartsWithDigit:
		def unapply(s: String): Option[(Char, String)] =
			if s.nonEmpty && s.head.isDigit then
				Some(s.head -> s.tail)
			else None

	private object EndsWithDigit:
		def unapply(s: String): Option[(String, Char)] =
			if s.nonEmpty && s.last.isDigit then
				Some(s.init -> s.last)
			else None

	private object StartsWithOneTwoSix:
		def unapply(s: String): Option[(Char, String)] =
			if s.length < 3 then
				None
			else
				s.substring(0, 3) match
					case "one" => Some('1' -> s.substring(3))
					case "two" => Some('2' -> s.substring(3))
					case "six" => Some('6' -> s.substring(3))
					case _  => None

	private object EndsWithOneTwoSix:
		def unapply(s: String): Option[(String, Char)] =
			if s.length < 3 then
				None
			else
				s.takeRight(3) match
					case "one" => Some(s.dropRight(3) -> '1')
					case "two" => Some(s.dropRight(3) -> '2')
					case "six" => Some(s.dropRight(3) -> '6')
					case _ => None

	private object StartsWithFourFiveNine:
		def unapply(s: String): Option[(Char, String)] =
			if s.length < 4 then
				None
			else
				s.substring(0, 4) match
					case "four" => Some('4' -> s.substring(4))
					case "five" => Some('5' -> s.substring(4))
					case "nine" => Some('9' -> s.substring(4))
					case _ => None

	private object EndsWithFourFiveNine:
		def unapply(s: String): Option[(String, Char)] =
			if s.length < 4 then
				None
			else
				s.takeRight(4) match
					case "four" => Some(s.dropRight(4) -> '4')
					case "five" => Some(s.dropRight(4) -> '5')
					case "nine" => Some(s.dropRight(4) -> '9')
					case _ => None

	private object StartsWithThreeSevenEight:
		def unapply(s: String): Option[(Char, String)] =
			if s.length < 5 then
				None
			else
				s.substring(0, 5) match
					case "three" => Some('3' -> s.substring(5))
					case "seven" => Some('7' -> s.substring(5))
					case "eight" => Some('8' -> s.substring(5))
					case _ => None

	private object EndsWithThreeSevenEight:
		def unapply(s: String): Option[(String, Char)] =
			if s.length < 5 then
				None
			else
				s.takeRight(5) match
					case "three" => Some(s.dropRight(5) -> '3')
					case "seven" => Some(s.dropRight(5) -> '7')
					case "eight" => Some(s.dropRight(5) -> '8')
					case _ => None

	def replaceNumberWords(line: String): String = {//one, two, six / four, five, nine / three, seven, eight
		@tailrec
		def scanL(result: StringBuilder, remaining: String): String = {
			if(remaining.isEmpty) {
				result.toString()
			} else {
				remaining match {
					case StartsWithDigit(d, tail) => result.append(d).toString()
					case StartsWithOneTwoSix(d, tail) => result.append(d).toString()
					case StartsWithFourFiveNine(d, tail) => result.append(d).toString()
					case StartsWithThreeSevenEight(d, tail) => result.append(d).toString()
					case other => scanL(result, other.substring(1))
				}
			}
		}

		@tailrec
		def scanR(result: StringBuilder, remaining: String): String = {
			if (remaining.isEmpty) {
				result.toString()
			} else {
				remaining match {
					case EndsWithDigit(init, d) => result.append(d).toString()
					case EndsWithOneTwoSix(init, d) => result.append(d).toString()
					case EndsWithFourFiveNine(init, d) => result.append(d).toString()
					case EndsWithThreeSevenEight(init, d) => result.append(d).toString()
					case other => scanR(result, other.dropRight(1))
				}
			}
		}
		scanL(StringBuilder(), line) + scanR(StringBuilder(), line)
	}

	def puzzle2(lines: Iterator[String]): Long = {
		//FIXME find the first number from left and the first one from right. "threethreeight" should give 38, not 33.
		lines.map(line => replaceNumberWords(line).filter(_.isDigit)).foldLeft(0L) { (total, line) =>
			total + s"${line.head}${line.last}".toLong
		}
	}

	def main(args: Array[String]): Unit = {

		getInputLines(2023, 1) match
			case Right(lines) =>
				val inputData = lines.toSeq
				println(s"Puzzle 1 = ${puzzle1(inputData.iterator)}")
				println(s"Puzzle 2 = ${puzzle2(inputData.iterator)}")
			case Left(err) =>
				println(s"Puzzle input didn't load ! Reason:\n $err")
	}

}
