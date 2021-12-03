package org.jro.adventofcode.y2020

import org.jro.adventofcode

import scala.util.matching.Regex

object Day5 extends App {

	case class Seat(row: Byte, col: Byte) {
		lazy val id = row * 8 + col
	}

	object Seat {
		val RowAndCol: Regex = """([FB]{7})([RL]{3})""".r

		def toByte(value: String, one: Char): Byte = {
			java.lang.Byte.parseByte(value.map(c => if(c == one) '1' else '0'), 2)
		}

		def toRowNum(value: String): Byte = {
			toByte(value, 'B')
		}

		def toColNum(value: String): Byte = {
			toByte(value, 'R')
		}

		def fromPartionString(value: String): Option[Seat] = value match {
			case RowAndCol(rowStr, colStr) =>
				val row = toRowNum(rowStr)
				val col = toColNum(colStr)
				Option(Seat(row, col))
			case _ =>
				println(s"""ERROR: Not a valid seat: "$value" !""")
				None
		}
	}

	adventofcode.getInputSourceOf(2020, 5).map(_.getLines().toIndexedSeq).foreach { lines =>
		val seatsIds = lines.flatMap(Seat.fromPartionString).map(_.id)
		val res1 = seatsIds.max

		val orderedSeatsIds = seatsIds.sorted
		val res2 = orderedSeatsIds.sliding(2).filter(step => (step(1) - step.head) == 2).toIndexedSeq

		println(s"res1 = $res1\nres2 = $res2")
	}


}
