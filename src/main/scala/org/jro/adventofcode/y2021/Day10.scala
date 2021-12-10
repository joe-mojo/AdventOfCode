package org.jro.adventofcode.y2021

import org.jro.adventofcode

import scala.collection.mutable
import scala.collection.Map

object Day10 {
	sealed trait InputError{
		def input: String
	}
	case class ParseError(override val input: String, at: Int, found: String, expected: String) extends InputError {
		override val toString: String = s"""Parse error for input "$input" at $at\n\tFound: "$found"\texpected: "$expected""""
	}
	case class IncompleteInput(override val input: String, remaingOpened: String) extends InputError


	val opening = IndexedSeq('<', '(', '[', '{')
	val closing = IndexedSeq('>', ')', ']', '}')

	val parseErrScoreMap = Map(
		')' -> 3L,
		']' -> 57L,
		'}' -> 1197L,
		'>' -> 25137L
	)
	val incompleteInputScoreMap = Map(
		')' -> 1L,
		']' -> 2L,
		'}' -> 3L,
		'>' -> 4L
	)



	def checkLine(line: String): Option[InputError] = {
		val bracketStack = mutable.Stack.empty[Char]
		val maybeLineErr: Option[InputError] = line.zipWithIndex.foldLeft(Option.empty[InputError]) { (status, charAndIndex) =>
			if(status.nonEmpty) status
			else{
				val (currentChar, currentIndex) = charAndIndex
				if(opening.contains(currentChar)) {
					bracketStack.push(currentChar)
					status
				} else  { //closing char
					bracketStack.headOption.toRight(
						IncompleteInput(line, "")
					).flatMap { topChar =>
						val corresponding = opening(closing.indexOf(currentChar))
						if(corresponding == topChar) {
							Right(bracketStack.pop())
						} else {
							Left(ParseError(line, currentIndex, s"$currentChar", s"${closing(opening.indexOf(topChar))}"))
						}
					}.left.toOption
				}
			}
		}
		maybeLineErr.orElse{
			if(bracketStack.isEmpty) None
			else Some(IncompleteInput(line, bracketStack.mkString("")))
		}
	}

	def parseErrScore(err: InputError): Long = {
		err match {
			case ParseError(_, _, found, _) => parseErrScoreMap(found.head)
			case IncompleteInput(_, _) => 0L
		}
	}

	def incompleteInputScore(err: InputError): Long = {
		err match {
			case iierr@IncompleteInput(_, _) =>
				fix(iierr).foldLeft(0L){ (total, c) =>
					val score = total * 5 + incompleteInputScoreMap(c)
					score
				}
			case _ => 0
		}
	}

	def fix(err: IncompleteInput): String = {
		err.remaingOpened.map(c => closing(opening.indexOf(c)))
	}

	val inputLines: Either[adventofcode.Error, Seq[String]] = adventofcode.getInputLines(2021, 10).map(_.toSeq)

	def puzzle(lines: Seq[String], scorer: InputError => Long): Long = {
		lines.flatMap(checkLine).map(scorer).sum
	}

	def puzzle2(lines: Seq[String], unitScorer: InputError => Long): Long = {
		val lineScores = lines.flatMap(checkLine).filter{
			case err: IncompleteInput => true
			case _ => false
		}.map(unitScorer).sorted
		println(s"length: ${lineScores.length}")
		println(s"middle index: ${lineScores.length / 2}")
		println(lineScores)
		lineScores(lineScores.length / 2)
	}

	def main(args: Array[String]): Unit = {
		val sample = """[({(<(())[]>[[{[]{<()<>>
					   |[(()[<>])]({[<{<<[]>>(
					   |{([(<{}[<>[]}>{[]{[(<()>
					   |(((({<>}<{<{<>}{[]{[]{}
					   |[[<[([]))<([[{}[[()]]]
					   |[{[{({}]{}}([{[{{{}}([]
					   |{<[[]]>}<{[{[{[]{()[[[]
					   |[<(<(<(<{}))><([]([]()
					   |<{([([[(<>()){}]>(<<{{
					   |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.split("\\n").toSeq
		println(s"Sample: ${puzzle(sample, parseErrScore)}") //OK
		println(s"Day 10.1: ${inputLines.map(puzzle(_, parseErrScore))}") //OK
		val sample2 =
			"""[({(<(())[]>[[{[]{<()<>>
			  |[(()[<>])]({[<{<<[]>>(
			  |(((({<>}<{<{<>}{[]{[]{}
			  |{<[[]]>}<{[{[{[]{()[[[]
			  |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.split("\\n").toSeq
		println(s"Sample2: ${puzzle2(sample2, incompleteInputScore)}")
		println(s"Day 10.2: ${inputLines.map(puzzle2(_, incompleteInputScore))}")
	}
}
