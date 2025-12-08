package org.jro.adventofcode.y2025

import scala.collection.mutable.ListBuffer
import org.jro.adventofcode
import org.jro.adventofcode.Error
import org.jro.adventofcode.InputData

object Day6 {
	private[y2025] enum Operator(symbol: Char, val neutral: Int) {
		case Add extends Operator('+', 0)
		case Multiply extends Operator('*', 1)

		def apply(a: Int, b: Int): Long = apply(a.toLong, b.toLong)

		def apply(a: Long, b: Long): Long = this match {
			case Add => a + b
			case Multiply => a * b
		}
	}

	private[y2025] object Operator{
		def unapply(input: String): Option[Operator] = input match {
			case "+" => Some(Operator.Add)
			case "*" => Some(Operator.Multiply)
			case _   => None
		}
	}

	private[y2025] case class Problem(operands: List[Int], operator: Operator) {
		def solve: Long = {
			operands.foldLeft(operator.neutral.toLong)((total, newOperand) => operator.apply(total, newOperand.toLong))
		}
	}

	private[y2025] def parseInput1(input: Iterator[String]): Seq[Problem] = {
		val lines = input.map(line => line.split("""\s+""").filter(_.nonEmpty)).toIndexedSeq
		val operators = lines.last.flatMap(Operator.unapply).toSeq
		val columns: Seq[List[Int]] = lines.init.transpose.map(col => col.map(_.toInt).toList)
		operators.zip(columns).map { case (operator, operands) => Problem(operands, operator) }
	}

	def puzzle1(input: Iterator[String]): Long = {
		parseInput1(input).map(_.solve).sum
	}

	private[y2025] def parseInput2(input: Iterator[String]): Seq[Problem] = {
		val lines = input.toIndexedSeq
		val operators = lines.last.split("""\s+""").filter(_.nonEmpty).flatMap(Operator.unapply).toSeq
		lines.init.transpose.reverse.map(_.filter(_.isDigit).mkString).foldLeft(List(List.empty[Int])){ (res, elem) =>
			if(elem.nonEmpty) {
				(elem.toInt :: res.head) :: res.tail
			}else{
				List.empty[Int] :: res
			}
		}.zip(operators).map({ case (operands, operator) => Problem(operands, operator) })
	}

	def puzzle2(input: Iterator[String]): Long = {
		parseInput2(input).map(_.solve).sum
	}

	def main(args: Array[String]): Unit =
		adventofcode.y2025.classicMain(6, puzzle1/*OK: 7644505810277*/, puzzle2/*OK: 12841228084455 */)
}
