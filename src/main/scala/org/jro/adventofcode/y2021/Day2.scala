package org.jro.adventofcode.y2021
import org.jro.adventofcode.{Error, getInputData}
import org.jro.adventofcode.Error.{NaN, WrongFixedSplit}

import scala.util.Try

trait SubmarineControl {
	def x: Int
	def depth: Int
	def forward(dx: Int): SubmarineControl
	def down(dDepth: Int): SubmarineControl
	def up(dDepth: Int): SubmarineControl
	def magic: Int = this.x * this.depth
}

case class SubmarinePos(override val x: Int = 0, override val depth: Int = 0) extends SubmarineControl {
	override def forward(dx: Int): SubmarinePos = SubmarinePos(this.x + dx, this.depth)
	override def down(dDepth: Int): SubmarinePos = SubmarinePos(this.x, this.depth + dDepth)
	override def up(dDepth: Int): SubmarinePos = SubmarinePos(this.x, this.depth - dDepth)
}

case class Submarine(override val x: Int = 0, override val depth: Int = 0, aim: Int = 0) extends SubmarineControl {
	override def forward(dx: Int): Submarine = this.copy(x = this.x + dx, depth = this.depth + this.aim * dx)
	override def down(da: Int): Submarine = this.copy(aim = this.aim + da)
	override def up(da: Int): Submarine =this.copy(aim = this.aim - da)
}

sealed trait Command {
	def apply(sub: SubmarineControl, d: Int): SubmarineControl
}
object Command {
	case object Forward extends Command {
		override def apply(sub: SubmarineControl, d: Int): SubmarineControl = sub.forward(d)
	}
	case object Down extends Command {
		override def apply(sub: SubmarineControl, d: Int): SubmarineControl = sub.down(d)
	}
	case object Up extends Command {
		override def apply(sub: SubmarineControl, d: Int): SubmarineControl = sub.up(d)
	}
	case object Unknown extends Command {
		override def apply(sub: SubmarineControl, d: Int): SubmarineControl = sub
	}
	def of(name: String): Command = {
		name match {
			case "forward" => Forward
			case "down" => Down
			case "up" => Up
			case _ => Unknown
		}
	}
}

object Day2 extends App {

	def parseLine(line: String): Either[Error, (Command, Int)] = {
		val splitLine = line.split(" ")
		if (splitLine.length != 2) Left(WrongFixedSplit(line, " ".r, 2, splitLine.length))
		else Try(splitLine(1).toInt).toEither.left.map(err =>
			NaN(splitLine(1))
		).map(n => (Command.of(splitLine.head), n))
	}

	def puzzle(subMarineGen: => SubmarineControl): Either[Error, Int] = {
		getInputData(2021, 2).flatMap { inputData =>
			Error.sequence(inputData.source.getLines().map(parseLine)).map { commands =>
				commands.foldLeft[SubmarineControl](subMarineGen){ (sub, args) =>
					args._1(sub, args._2)
				}
			}.map{finalSub =>
				println(s"Final submarine position: ${finalSub}")
				finalSub.magic
			}
		}
	}


	println(s"Day2 part 1 : ${puzzle(SubmarinePos())}")
	println(s"Day2 part 2 : ${puzzle(Submarine())}")

}
