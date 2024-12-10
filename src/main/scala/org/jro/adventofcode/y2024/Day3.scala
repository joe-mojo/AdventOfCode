package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.IOError

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Using
import scala.util.matching.Regex

/**
 * @author joe_mojo.
 *         2024/12/10
 */
object Day3 {
	private val MulRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r
	private val DoRegex = """do\(\)""".r
	private val DoNotRegex = """don't\(\)""".r

	def puzzle1(input: String): Int = {
		MulRegex.findAllMatchIn(input).map { regexMatch =>
			val a = regexMatch.group(1).toInt
			val b = regexMatch.group(2).toInt
			a * b
		}.sum
	}

	sealed trait Mul {
		def apply(a: Int, b: Int): Int = a * b
	}
	object Mul {
		case object Do extends Mul
		case object DoNot extends Mul {
			override def apply(a: Int, b: Int): Int = 0
		}
	}
	
	private def findPattern(input: String, pattern: Regex): Array[Regex.Match] = {
		pattern.findAllMatchIn(input).toArray
	}

	private def findPatternsPar(input: String)(implicit ec: ExecutionContext): Future[Seq[Regex.Match]] = {
		val mulMatchesFuture = Future(findPattern(input, MulRegex))
		val doMatchesFuture = Future(findPattern(input, DoRegex))
		val doNotMatchesFuture = Future(findPattern(input, DoNotRegex))

		for {
			mulMatches <- mulMatchesFuture
			doMatches <- doMatchesFuture
			doNotMatches <- doNotMatchesFuture
		} yield (mulMatches ++ doMatches ++ doNotMatches).sortBy(_.start).toIndexedSeq
	}

	def puzzle2(input: String)(implicit ec: ExecutionContext): Int = {
		val futureRes: Future[(Int, Mul)] = findPatternsPar(input).map { matches =>
			matches.foldLeft((0, Mul.Do): (Int, Mul)) { (acc, matchRegex) =>
				val (accValue, accMul) = acc
				matchRegex.matched match {
					case MulRegex(a, b) => (accValue + accMul.apply(a.toInt, b.toInt), accMul)
					case DoRegex() => (accValue, Mul.Do)
					case DoNotRegex() => (accValue, Mul.DoNot)
				}
			}
		}
		Await.result(futureRes.map(_._1), Duration.Inf)
	}

	def main(args: Array[String]): Unit = {
		implicit val ec: ExecutionContext = ExecutionContext.global
		adventofcode.getInputData(2024, 3).flatMap { inputData =>
			Using(inputData.source) { source =>
				source.mkString
			}.toEither.left.map(throwable => IOError(inputData.resource, throwable))
		} match
			case Right(content) =>
				println(s"Puzzle 1 = ${puzzle1(content)}") // 175015740 OK
				println(s"Puzzle 2 = ${puzzle2(content)}") // 112272912 OK
			case Left(error) =>
				println(s"Error: $error")
	}
}
