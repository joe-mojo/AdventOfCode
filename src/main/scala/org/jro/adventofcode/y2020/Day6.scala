package org.jro.adventofcode.y2020

import org.jro.adventofcode

import scala.util.matching.Regex

object Day6 {

	case class QGroup(persons: Seq[String]) {
		lazy val uniqueYesAnswers: Set[Char] = persons.foldLeft(Set.empty[Char]) (_ ++ _)

		lazy val unanymousAnswers: Set[Char] = {
			persons.map{ line =>
				line.foldLeft(Set.empty[Char])(_ + _)
			}.reduce(_ intersect _)
		}
	}
	object QGroup {
		val PersonSeparator: Regex = """\n""".r
		def fromTxt(groupTxt: String): QGroup = {
			QGroup(PersonSeparator.split(groupTxt))
		}
	}

	val GroupSeparator: Regex = """\n\n""".r
	def parseInput(txt: String): Seq[QGroup] = {
		GroupSeparator.split(txt).map(QGroup.fromTxt)
	}

	def puzzle1(inputTxt: String): Int = {
		parseInput(inputTxt).map(_.uniqueYesAnswers.size).sum
	}

	def puzzle2(inputTxt: String): Int = {
		parseInput(inputTxt).map(_.unanymousAnswers.size).sum
	}

	def main(args: Array[String]): Unit = {
		val maybeTxt = adventofcode.getInputSourceOf(2020, 6).map { src =>
			src.getLines().mkString("\n")
		}

		val res1 = maybeTxt.map(puzzle1)
		val res2 = maybeTxt.map(puzzle2)

		println(s"res1 = $res1 ; res2 = $res2")
	}

}
