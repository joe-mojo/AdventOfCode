package org.jro.adventofcode.y2019

import scala.collection.mutable
import scala.reflect.ClassTag._


case class Program(code: IndexedSeq[Int]) {
	def run: Program = {
		val state = ProgramState.start(this, code(1), code(2))
		while(state.next()){}
		state.result()
	}
	def run(noun: Int, verb: Int): Program = {
		val state = ProgramState.start(this, noun, verb)
		while(state.next()){}
		state.result()
	}

	def output: Int = code(0)
}

case class ProgramState(code: mutable.IndexedSeq[Int], var ip: Int) {
	def next(): Boolean = {
		if(ip >= 0 && ip < code.length || ip > 0 && code(ip - 1) != 99) {
			code(ip) match {
				case 1 =>
					code(code(ip + 3)) = code(code(ip + 1)) + code(code(ip +2))
					ip = ip + 4
					true
				case 2 =>
					code(code(ip + 3)) = code(code(ip + 1)) * code(code(ip + 2))
					ip = ip + 4
					true
				case 99 =>
					ip = ip + 1
					false
			}
		} else false
	}

	def result(): Program = Program((Vector.newBuilder[Int] ++= code).result())
}

object ProgramState {
	def start(program: Program, noun: Int, verb: Int): ProgramState = {
		ProgramState((mutable.ArrayBuilder.make[Int] ++= program.code).result().updated(1, noun).updated(2, verb), 0)
	}
}


object Day2 extends App {
	val input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,5,23,1,6,23,27,1,27,5,31,2,31,10,35,2,35,6,39,1,39,5,43,2,43,9,47,1,47,6,51,1,13,51,55,2,9,55,59,1,59,13,63,1,6,63,67,2,67,10,71,1,9,71,75,2,75,6,79,1,79,5,83,1,83,5,87,2,9,87,91,2,9,91,95,1,95,10,99,1,9,99,103,2,103,6,107,2,9,107,111,1,111,5,115,2,6,115,119,1,5,119,123,1,123,2,127,1,127,9,0,99,2,0,14,0"

	val program = Program(input.split(",").toIndexedSeq.map(_.toInt))
	println(s"Program = $program")

	val result = program.run(12, 2)
	println(s"Result program = $result")

	//Find noun and verb giving 19690720 as output
	val (resNoun, resVerb, targetResult) = (for {
		noun <- (0 to 99).iterator
		verb <- (0 to 99).iterator
		result = program.run(noun, verb).output
	} yield {
		(noun, verb, result)
	}).dropWhile(_._3 != 19690720).takeWhile(_._3 == 19690720).toList.head

	println(s"for $targetResult found for noun = $resNoun and verb = $resVerb")
	println(s"Answer is ${100 * resNoun + resVerb}")
}
