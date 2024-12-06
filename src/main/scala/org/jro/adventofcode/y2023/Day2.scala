package org.jro.adventofcode.y2023

import org.jro.adventofcode
import org.jro.adventofcode.getInputLines

import java.lang.Math.max


sealed trait TricolorDraw:
	def red: Int
	def green: Int
	def blue: Int
	def power: Int = red * green * blue

case class CubeDraw(override val red: Int, override val green: Int, override val blue: Int) extends TricolorDraw {
	def isPossibleWIth(total: TricolorDraw): Boolean = red <= total.red && green <= total.green && blue <= total.blue
}

case object CubesTotal extends TricolorDraw {
	override val red: Int = 12
	override val green: Int = 13
	override val blue: Int = 14
}

case class Game(id: Int, draws: Seq[CubeDraw]) {
	def isPossibleWith(total: TricolorDraw): Boolean = draws.forall(_.isPossibleWIth(total))
	def minimumTotal: TricolorDraw = draws.reduce { (cd1, cd2) =>
		CubeDraw(max(cd1.red, cd2.red), max(cd1.green, cd2.green), max(cd1.blue, cd2.blue))
	}
}

/**
 * @author joe_mojo.
 *         2023/12/02
 */
object Day2 {
	private val GameIdRegex = """^Game (\d+):(.*)$""".r
	private val CubeCountRegex = """^(\d+) (red|green|blue)""".r

	private[y2023] def parseGame(line:String): Game = {
		line match
			case GameIdRegex(id, drawsLine) => Game(id.toInt, parseDraws(drawsLine))
	}

	private[y2023] def parseDraws(drawsLine: String): Seq[CubeDraw] = {
		drawsLine.split(";").toIndexedSeq.map(parseDraw)
	}

	private[y2023] def parseDraw(draw: String): CubeDraw = {
		val colorMap: Map[String, Int] = draw.split(",").map { drawElt =>
			drawElt.trim match
				case CubeCountRegex(count, color) => color -> count.toInt
		}.toMap
		CubeDraw(colorMap.getOrElse("red", 0), colorMap.getOrElse("green", 0), colorMap.getOrElse("blue", 0))
	}

	def puzzle1(lines: Iterable[String]): Int = {
		lines.map(parseGame).filter(_.isPossibleWith(CubesTotal)).map(_.id).sum
	}
	def puzzle2(lines: Iterable[String]): Int = {
		lines.map(parseGame(_).minimumTotal.power).sum
	}

	def main(args: Array[String]): Unit = {
		val sample1 =
			"""Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
			  |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
			  |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
			  |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
			  |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin.split("\n")

		println(s"Puzzle1 on sample = ${puzzle1(sample1)}")
		println(s"Puzzle2 on sample = ${puzzle2(sample1)}")


		getInputLines(2023, 2) match
			case Right(lines) =>
				println(s"Puzzle1 = ${puzzle1(lines)}")
				println(s"Puzzle2 = ${puzzle2(lines)}")

			case Left(err) =>
				println(s"Puzzle input didn't load ! Reason:\n $err")

	}
}
