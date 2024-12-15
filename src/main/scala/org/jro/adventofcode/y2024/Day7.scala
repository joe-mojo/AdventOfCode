package org.jro.adventofcode.y2024

import org.jro.adventofcode
import org.jro.adventofcode.Error.IOError
import org.jro.adventofcode.y2024.Day7.Operator.{`*`, `+`}

import scala.annotation.{tailrec, targetName}
import scala.util.Using

/**
 * @author joe_mojo.
 *         2024/12/15
 */
object Day7 {
	case class PartialCalibration(expectedResult: Long, operands: List[Long])

	sealed trait Operator {
		infix def apply(operand1: Long, operand2: Long): Long
	}

	object Operator {
		@targetName("Plus")
		case object `+` extends Operator {
			override infix def apply(operand1: Long, operand2: Long): Long = operand1 + operand2
		}
		@targetName("Multiply")
		case object `*` extends Operator {
			override infix def apply(operand1: Long, operand2: Long): Long = operand1 * operand2
		}
		@targetName("Concatenate")
		case object `||` extends Operator {
			override infix def apply(operand1: Long, operand2: Long): Long = (s"$operand1" + s"$operand2").toLong
		}
		val all: Set[Operator] = Set(`+`, `*`, `||`)
	}

	case class OperationElement(operator: Operator, rightOperand: Long) {
		def apply(leftOperand: Long): Long = operator(leftOperand, rightOperand)
	}

	case class OperationExpression(leftOperand: Long, operations: Seq[OperationElement]) {
		def apply: Long = operations.foldLeft(leftOperand) { (leftOperand, operation) => operation(leftOperand) }
	}

	case class FullCalibration(result: Long, operationExpression: OperationExpression)

	private[y2024] def parseLine(line: String): PartialCalibration = {
		val calibrationParts = line.split(": ")
		val expectedResult = calibrationParts(0).toLong
		val operationExpressionParts = calibrationParts(1).split(" ")
		PartialCalibration(expectedResult, operationExpressionParts.map(_.toLong).toList)
	}

	private[y2024] def generateExpressions(availableOperators: Set[Operator])(calibration: PartialCalibration): Seq[OperationExpression] = {
		val firstOperand = calibration.operands.head
		def generateOps(operands: List[Long], ops: Seq[OperationElement]): Seq[OperationExpression] = {
			if(operands.isEmpty) {
				Seq(OperationExpression(firstOperand, ops))
			} else {
				availableOperators.toSeq.flatMap { operator =>
					generateOps(operands.tail, ops :+ OperationElement(operator, operands.head))
				}
			}
		}
		generateOps(calibration.operands.tail, Seq.empty)
	}


	private[y2024] def calibrationLookup(calibration: PartialCalibration, generateExpressions: PartialCalibration => Seq[OperationExpression]): Seq[FullCalibration] = {
		generateExpressions(calibration).filter(_.apply == calibration.expectedResult).map(FullCalibration(calibration.expectedResult, _))
	}

	private[y2024] def puzzle1(calibrations: Seq[PartialCalibration]): BigInt = {
		calibrations.flatMap { calibration =>
			calibrationLookup(calibration, generateExpressions(Set(`+`, `*`))).headOption // count only one result per calibration set !!!
		}.map(_.result).foldLeft(BigInt(0))(_ + _)
	}

	private[y2024] def puzzle2(calibrations: Seq[PartialCalibration]): BigInt = {
		calibrations.flatMap { calibration =>
			calibrationLookup(calibration, generateExpressions(Operator.all)).headOption // count only one result per calibration set !!!
		}.map(_.result).foldLeft(BigInt(0))(_ + _)
	}

	def main(args: Array[String]): Unit = {
		adventofcode.getInputData(2024, 7).flatMap { inputData =>
			Using(inputData.source) { source =>
				source.getLines().map(parseLine).toSeq
			}.toEither.left.map(throwable => IOError(inputData.resource, throwable))
		} match
			case Right(calibrations) =>
				println(s"Puzzle 1 = ${puzzle1(calibrations)}") // 10741443549536 OK
				println(s"Puzzle 2 = ${puzzle2(calibrations)}") // 500335179214836 OK (with a few seconds delay and some cpu fan acceleration '^^)
			case Left(error) =>
				println(s"Error: $error")
	}
}
