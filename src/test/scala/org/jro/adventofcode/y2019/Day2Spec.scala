package org.jro.adventofcode.y2019

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class Day2Spec extends AnyFreeSpec with Matchers {
	val exampleCode = IndexedSeq(1,9,10,3, 2,3,11,0, 99, 30,40,50)
	val exampleCodeAfterAdd = exampleCode.updated(3, 70)
	val exampleCodeAfterAddAndMul = exampleCodeAfterAdd.updated(0, 3500)

	"ProgramStep" - {
		"should add operands at index 1 and 2 and modify program state at index 3" in {
			val state = ProgramState((mutable.ArrayBuilder.make[Int] ++= exampleCode).result(), 0)
			val hasNext = state.next()
			hasNext shouldBe true
			state.code should contain theSameElementsInOrderAs exampleCodeAfterAdd
		}
		"should multiply operand at index 4 and 5 and modify programe state at index 0" in {
			val state = ProgramState((mutable.ArrayBuilder.make[Int] ++= exampleCodeAfterAdd).result(), 4)
			val hasNext = state.next()
			hasNext shouldBe true
			state.code should contain theSameElementsInOrderAs exampleCodeAfterAddAndMul
		}
	}

	"Program" - {
		s"with code $exampleCode" - {
			"should give right result" in {
				val prog = Program(exampleCode)
				val res = prog.run
				res.code should contain theSameElementsInOrderAs exampleCodeAfterAddAndMul
			}
		}

		val code1 = IndexedSeq(1,0,0,0,99)
		val expectedRes1 = IndexedSeq(2,0,0,0,99)
		s"with code $code1" - {
			"should give the right result" in {
				val prog = Program(code1)
				val res = prog.run
				res.code should contain theSameElementsInOrderAs expectedRes1
			}
		}

		val code2 = IndexedSeq(2,3,0,3,99)
		val expectedRes2 = IndexedSeq(2,3,0,6,99)
		s"with code $code2" - {
			"should give the right result" in {
				val prog = Program(code2)
				val res = prog.run
				res.code should contain theSameElementsInOrderAs expectedRes2
			}
		}

		val code3 = IndexedSeq(2,4,4,5,99,0)
		val expectedRes3 = IndexedSeq(2,4,4,5,99,9801)
		s"with code $code3" - {
			"should give the right result" in {
				val prog = Program(code3)
				val res = prog.run
				res.code should contain theSameElementsInOrderAs expectedRes3
			}
		}

		val code4 = IndexedSeq(1,1,1,4, 99, 5,6,0,99)
		val expectedRes4 = IndexedSeq(30,1,1,4, 2,5,6,0, 99)
		s"with code $code4" - {
			"should give the right result" in {
				val prog = Program(code4)
				val res = prog.run
				res.code should contain theSameElementsInOrderAs expectedRes4
			}
		}

	}

}
