package org.jro.adventofcode.y2020

import org.jro.adventofcode.{Error, getInputSourceOf}
import org.jro.adventofcode.Error.{NaN, UnknownEnumValue, WrongFixedSplit}
import org.jro.adventofcode.y2020.Day8.Operator.{Jmp, Nop}
import org.jro.adventofcode.y2020.Day8.Status.{InfiniteLoop, TerminatedOK}

import scala.util.Try
import scala.util.matching.Regex
import scala.collection.{mutable => m}

object Day8 {

	sealed trait Operator {
		val name: String = toString.toLowerCase
		def exec(frame: Frame, operand: Int): Frame
	}
	object Operator {
		case object Nop extends Operator {
			override def exec(frame: Frame, operand: Int): Frame  = frame.copy(nextIndex = frame.nextIndex + 1)
		}
		case object Acc extends Operator {
			override def exec(frame: Frame, operand: Int): Frame  = Frame(frame.accu + operand, frame.nextIndex + 1)
		}
		case object Jmp extends Operator {
			override def exec(frame: Frame, operand: Int): Frame = frame.copy(nextIndex = frame.nextIndex + operand)
		}

		def of(value: String): Option[Operator] = {
			value.toLowerCase match {
				case Nop.name => Option(Nop)
				case Acc.name => Option(Acc)
				case Jmp.name => Option(Jmp)
				case _ => None
			}
		}
	}
	case class Frame(accu: Int, nextIndex: Int)
	case class Instruction(operator: Operator, operand: Int) {
		def exec(frame: Frame): Frame = operator.exec(frame, operand)
	}
	object Instruction {
		val OnSpace: Regex = """ """.r
		def ofLine(value: String): Either[Error, Instruction] = {
			val ops = OnSpace.split(value.trim)
			if(ops.length < 2) Left(WrongFixedSplit(value, OnSpace, 2, ops.length))
			else {
				Operator.of(ops.head).fold[Either[Error, Operator]](
					Left(UnknownEnumValue[Operator](ops.head))
				)(Right(_)).flatMap { operator =>
					Try(ops(1).toInt).fold(_ => Left(NaN(ops(1))), Right(_)).map { operand =>
						Instruction(operator, operand)
					}
				}
			}
		}
	}

	case class Program(instructions: IndexedSeq[Instruction])
	object Program {
		def of(lines: Iterator[String]): Either[Error, Program] = {
			Error.sequence(lines.map(Instruction.ofLine)).map(instructions => Program(instructions.toIndexedSeq))
		}
	}

	sealed trait Status
	object Status {
		case object Runnable extends Status
		case class InfiniteLoop(repeatedIndex: Int) extends Status
		case object TerminatedOK extends Status
	}

	case class Process(program: Program) {
		private var frame: Frame = Frame(0, 0)
		private val alreadyExecuted: m.Set[Int] = m.Set.empty
		private var status: Status = Status.Runnable
		def getFrame: Frame = frame
		def getAlreadyExecuted: Set[Int] = alreadyExecuted.toSet
		def getStatus: Status = status
		def next(): Boolean = {
			if (alreadyExecuted.contains(frame.nextIndex)){
				status = InfiniteLoop(frame.nextIndex)
				false
			} else if(frame.nextIndex >= program.instructions.length) {
				status = TerminatedOK
				false
			}
			else {
				alreadyExecuted += frame.nextIndex
				frame = program.instructions(frame.nextIndex).exec(frame)
				true
			}
		}
		def show: String = {
			s"Process: A=${frame.accu} IP=${frame.nextIndex} (${if(frame.nextIndex < program.instructions.length) program.instructions(frame.nextIndex) else "<out of instructions>"}) status=$status"
		}
		def runAllSilently(): Status = {
			while(next())()
			status
		}
	}

	def main(args: Array[String]): Unit = {

		val maybeProgram: Either[Error, Program] = getInputSourceOf(2020, 8).map(_.getLines()).flatMap(Program.of)

		//Part 1
		val res1 = maybeProgram.map { prog =>
			val proc = Process(prog)
			print(s"Starting ${proc.show}")
			while(proc.next()){
				println(" -> OK")
				print(s"${proc.show}")
			}
			println(s" -> KO")
			println(s"Stopped ${proc.show} ")
			proc.getFrame.accu
		}

		println(s"res1 = $res1")

		//Part2
		val res2: Either[Error, Option[Int]] = maybeProgram.map { prog =>
			prog.instructions.zipWithIndex.filter {
				case (Instruction(Nop, operand), _) if operand != 0 => true
				case (Instruction(Jmp, _), _) => true
				case other => false
			}.map {
				case (inst@Instruction(Nop, _), index) =>
					Process(Program(prog.instructions.updated(index, inst.copy(operator = Jmp))))
				case (inst@Instruction(Jmp, _), index) =>
					Process(Program(prog.instructions.updated(index, inst.copy(operator = Nop))))
			}.find {proc =>
				proc.runAllSilently() == TerminatedOK
			}.map(_.getFrame.accu)
		}
		println(res2)
	}
}
