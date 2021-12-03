package org.jro.adventofcode.y2020

import org.jro.adventofcode.y2020.Day8.Operator._
import org.jro.adventofcode.y2020.Day8.{Instruction, Program}
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Day8Spec extends AnyFreeSpec with Matchers with Inside {
	private val sampleInput = """nop +0
						|acc +1
						|jmp +4
						|acc +3
						|jmp -3
						|acc -99
						|acc +1
						|jmp -4
						|acc +6
						|""".stripMargin
	private val expectedSampleProgram = Program(IndexedSeq(
		Instruction(Nop, 0),
		Instruction(Acc, 1),
		Instruction(Jmp, 4),
		Instruction(Acc, 3),
		Instruction(Jmp, -3),
		Instruction(Acc, -99),
		Instruction(Acc, 1),
		Instruction(Jmp, -4),
		Instruction(Acc, 6)
	))

	"Program lines, when" - {
		"parsed from sample input should" - {
			"give the expected Program" in {
				inside(Day8.Program.of(sampleInput.split("\n").iterator)) {
					case Right(Program(instructions)) =>
						instructions should contain theSameElementsInOrderAs expectedSampleProgram.instructions
				}
			}
		}
	}

}
