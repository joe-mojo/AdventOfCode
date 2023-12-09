package org.jro.adventofcode.y2023

import org.jro.adventofcode
import org.jro.adventofcode.getInputLinesOrThrow
import org.jro.adventofcode.y2023.Day5.*

case class Day5(seeds: Set[Int], steps: Seq[Step]) {

}

object Day5 {

	case class Step(mappings: Set[Mapping]) {
		def map(srcValue: Long): Long = {
			???
		}
	}
	case class Mapping(dstStart: Int, srcStart: Int, range: Long) extends PartialFunction[Long, Long] {
		val dstEnd: Long = dstStart + range - 1
		val srcEnd: Long = srcStart + range - 1
		val offset: Long = dstStart - srcStart

		override def apply(srcValue: Long): Long = {
			if(srcValue >= srcStart && srcValue <= srcEnd) srcValue + offset
			else srcValue
		}


		override def isDefinedAt(value: Long): Boolean = {
			???
		}
	}


	def main(args: Array[String]): Unit = {
		val lines = getInputLinesOrThrow(2023, 5)
	}
}
